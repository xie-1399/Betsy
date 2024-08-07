package Betsy

import Betsy.Architecture.{getActivationBusConfig, getWeightBusConfig, maxTransLen}
import spinal.core._
import spinal.lib._
import BetsyLibs._
import Betsy.Until._
import spinal.lib.bus.amba4.axi.{Axi4, Axi4Config}

/**
 ** Betsy follow the MiT Licence.(c) xxl, All rights reserved **
 ** Update Time : 2024/5/31      SpinalHDL Version: 1.94       **
 ** You should have received a copy of the MIT License along with this library **
 ** integration the compute/control/data move module in the BetsyNPU **
 ** The ML activations, average and maximum pooling, normalization, and image resizing use SIMD instruction.
 ** Some ML operations, such as padding, are achieved by changing the memory layout. **
 */

class Top[T <: Data with Num[T]](gen:HardType[T],arch: Architecture,log:Boolean = false,initContent:Array[BigInt] = null) extends BetsyModule{

  val instructionLayOut = InstructionLayOut(arch,gen = log)
  require(gen.getBitsWidth == arch.dataWidth,"the clare data width is not match in the arch !!! ")
  require(gen().getClass.toString.split("\\.").last == arch.dataKind , "the clare data type is not match in the arch !!!")

  val io = new Bundle{
     val weightBus = master(Axi4(getWeightBusConfig(arch)))
     val activationBus = master(Axi4(getActivationBusConfig(arch)))
     val instruction = slave Stream Bits(instructionLayOut.instructionSizeBytes * 8 bits)
  }
  val decode = new Decode(arch)(instructionLayOut)
  val scratchPad = new DualPortMem(Vec(gen,arch.arraySize),arch.localDepth,initContent = initContent) // no mask with
  val systolicArray = new SystolicArray(gen,height = arch.arraySize,width = arch.arraySize)
  val accumulatorWithALUArray = new AccumulatorWithALUArray(gen,arch)
  val localRouter = new LocalRouter(Vec(gen,arch.arraySize),arch)
  val hostRouter = new HostRouter(Vec(gen,arch.arraySize))

  /* external bus trans like : (data width) * (array size) as one burst trans for the Band width restrict */
  /* for example trans 8 * 64 with bandwidth 256  -> len = 2 for one dram data exchange into the local
  *  and then don't use the axi outstanding buffer to translate */

  val Dram = new Composite(this,"Dram"){
    val len = U((arch.arraySize * arch.dataWidth) / arch.bandWidth, 8 bits)
    val size = U(log2Up(arch.bandWidth / 8), 3 bits)
    val rspCounter = Counter(maxTransLen).init(0)
    val rspPayload = Reg(Bits(arch.arraySize * arch.dataWidth bits)).init(0)

    val dram0 = decode.io.dram0.toAxi4(
      axi4Config = getWeightBusConfig(arch),
      arValid = decode.io.dram0.valid && (!decode.io.dram0.write),
      awValid = decode.io.dram0.valid && decode.io.dram0.write,
      address = decode.io.dram0offset.offset,
      size = size,
      len = len - 1,
      data = hostRouter.io.dram0.dataOut.payload.asBits
    )
    val dram1 = decode.io.dram1.toAxi4(
      axi4Config = getWeightBusConfig(arch),
      arValid = decode.io.dram1.valid && (!decode.io.dram1.write),
      awValid = decode.io.dram1.valid && decode.io.dram1.write,
      address = decode.io.dram1offset.offset,
      size = size,
      len = len - 1,
      data = hostRouter.io.dram1.dataOut.payload.asBits
    )

    when(dram0.r.fire){
      rspCounter.increment()
      rspPayload.subdivideIn(arch.bandWidth bits)(rspCounter.resized) := dram0.r.payload.data
    }
    when(dram1.r.fire){
      rspCounter.increment()
      rspPayload.subdivideIn(arch.bandWidth bits)(rspCounter.resized) := dram1.r.payload.data
    }
    when(dram0.r.last || dram1.r.last){
      rspCounter.clear()
    }
    dram0 >> io.weightBus
    dram1 >> io.activationBus
    decode.io.dram0.ready := dram0.r.last || dram0.b.fire
    decode.io.dram1.ready := dram1.r.last || dram1.b.fire

    hostRouter.io.dram0.dataIn.valid := RegNext(dram0.r.last).init(False)
    hostRouter.io.dram0.dataIn.payload.assignFromBits(rspPayload.asBits)
    hostRouter.io.dram0.dataOut.ready := dram0.b.fire

    hostRouter.io.dram1.dataIn.valid := RegNext(dram1.r.last).init(False)
    hostRouter.io.dram1.dataIn.payload.assignFromBits(rspPayload.asBits)
    hostRouter.io.dram1.dataOut.ready := dram1.b.fire
  }

  /* Betsy connection */
  val Betsy = new Composite(this,"NPU") {
    decode.io.instructionFormat.arbitrationFrom(io.instruction)
    decode.io.instructionFormat.payload := InstructionFormat.fromBits(io.instruction.payload)(instructionLayOut)
    /* decode out */
    localRouter.io.control << decode.io.localDataFlow
    hostRouter.io.control << decode.io.hostDataFlow
    systolicArray.io.control << decode.io.systolicArrayControl
    accumulatorWithALUArray.io.control << decode.io.accumulatorWithALUArrayControl
    scratchPad.io.portA.control << decode.io.memPortA
    scratchPad.io.portB.control << decode.io.memPortB

    /* local router connect */
    localRouter.io.accumulatorDataFlow.accIn >> accumulatorWithALUArray.io.inputs
    localRouter.io.accumulatorDataFlow.accOut << accumulatorWithALUArray.io.outputs
    localRouter.io.arrayDataFlow.weight >> systolicArray.io.weight
    localRouter.io.arrayDataFlow.input >> systolicArray.io.input
    localRouter.io.arrayDataFlow.output << systolicArray.io.output
    localRouter.io.memoryDataFlow.memOut << scratchPad.io.portA.dataOut
    localRouter.io.memoryDataFlow.memIn >> scratchPad.io.portA.dataIn

    /* host Router */
    hostRouter.io.mem.dataIn >> scratchPad.io.portB.dataIn
    hostRouter.io.mem.dataOut << scratchPad.io.portB.dataOut
  }

}