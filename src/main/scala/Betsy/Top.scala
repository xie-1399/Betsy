package Betsy

import Betsy.Architecture.{getActivationBusConfig, getWeightBusConfig}
import spinal.core._
import spinal.lib._
import BetsyLibs._
import Betsy.Until._
import spinal.lib.bus.amba4.axi.Axi4

/**
 ** Betsy follow the MiT Licence.(c) xxl, All rights reserved **
 ** Update Time : 2024/5/31      SpinalHDL Version: 1.94       **
 ** You should have received a copy of the MIT License along with this library **
 ** integration the compute/control/datamove module in the BetsyNPU **
 ** The ML activations, average and maximum pooling, normalization, and image resizing use SIMD instruction.
 ** Some ML operations, such as padding, are achieved by changing the memory layout. **
 */


class Top[T <: Data with Num[T]](gen:HardType[T],arch: Architecture,log:Boolean = false,initContent:Array[BigInt] = null) extends BetsyModule{

  val instructionLayOut = InstructionLayOut(arch,gen = log)
  require(gen.getBitsWidth == arch.dataWidth,"the clare data width is not match in the arch !!! ")
  require(gen().getClass.toString.split("\\.").last == arch.dataKind , "the clare data type is not match in the arch !!!")

  val io = new Bundle{
     // val weightBus = master(Axi4(getWeightBusConfig(arch)))
     // val activationBus = master(Axi4(getActivationBusConfig(arch)))
    val dram0 = master(BetsyStreamPass(Vec(gen(),arch.arraySize)))
    val dram1 = master(BetsyStreamPass(Vec(gen(),arch.arraySize)))
    val instruction = slave Stream Bits(instructionLayOut.instructionSizeBytes * 8 bits)
  }
  val decode = new Decode(arch)(instructionLayOut)
  val scratchPad = new DualPortMem(Vec(gen,arch.arraySize),arch.localDepth,initContent = initContent) // no mask with
  val systolicArray = new SystolicArray(gen,height = arch.arraySize,width = arch.arraySize)
  val accumulatorWithALUArray = new AccumulatorWithALUArray(gen,arch)
  val localRouter = new LocalRouter(Vec(gen,arch.arraySize),arch)
  val hostRouter = new HostRouter(Vec(gen,arch.arraySize))

  /* Betsy connection */
  val Betsy = new Area {

    decode.io.instruction.arbitrationFrom(io.instruction)
    decode.io.instruction.payload := InstructionFormat.fromBits(io.instruction.payload)(instructionLayOut)
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
    hostRouter.io.dram0 <> io.dram0
    hostRouter.io.dram1 <> io.dram1

    decode.io.dram0.ready := True
    decode.io.dram1.ready := True
  }

}

object Top extends App{
  SpinalSystemVerilog(new Top(SInt(8 bits),Architecture.tiny()))
}