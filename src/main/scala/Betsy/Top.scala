package Betsy

import spinal.core._
import spinal.lib._
import BetsyLibs._
import Betsy.Until._

/**
 ** Betsy follow the MiT Licence.(c) xxl, All rights reserved **
 ** Update Time : 2024/5/31      SpinalHDL Version: 1.94       **
 ** You should have received a copy of the MIT License along with this library **
 ** integration the compute/control/datamove module in the BetsyNPU**
 */

/* the npu top temporal */

class Top[T <: Data with Num[T]](gen:HardType[T],arch: Architecture,log:Boolean = false,initContent:Array[BigInt] = null) extends BetsyModule{

  val instructionLayOut = InstructionLayOut(arch,gen = log)
  require(gen.getBitsWidth == arch.dataWidth,"the clare data width is not match in the arch !!! ")
  require(gen().getClass.toString.split("\\.").last == arch.dataKind , "the clare data type is not match in the arch !!!")

  val io = new Bundle{
    // val dram0 = master(BetsyStreamPass(gen))
    // val dram1 = master(BetsyStreamPass(gen))
    val instruction = slave Stream Bits(instructionLayOut.instructionSizeBytes * 8 bits)
  }

  val decode = new Decode(arch)(instructionLayOut)
  decode.io.instruction.payload := InstructionFormat.fromBits(io.instruction.payload)(instructionLayOut)
  decode.io.instruction.arbitrationFrom(io.instruction)
  // val hostRouter = new HostRouter(gen) /* the Host Router is connected to the PortB */
  val scratchPad = new DualPortMem(Vec(gen,arch.arraySize),arch.localDepth,initContent = initContent) // no mask with
  scratchPad.io.portA.control << decode.io.memPortA
  scratchPad.io.portB.blockPort()

  val systolicArray = new SystolicArray(gen,height = arch.arraySize,width = arch.arraySize)
  val localRouter = new LocalRouter(Vec(gen,arch.arraySize),arch)

  systolicArray.io.control << decode.io.systolicArrayControl
  systolicArray.io.weight << localRouter.io.arrayDataFlow.weight
  systolicArray.io.input << localRouter.io.arrayDataFlow.input
  systolicArray.io.output >> localRouter.io.arrayDataFlow.output

  localRouter.io.control << decode.io.localDataFlow
  localRouter.io.memoryDataFlow.memOut << scratchPad.io.portA.dataOut
  localRouter.io.memoryDataFlow.memIn >> scratchPad.io.portA.dataIn

  localRouter.io.accumulatorDataFlow.accOut.valid := False
  localRouter.io.accumulatorDataFlow.accIn.ready := False
  // hostRouter.io.mem.dataIn << scratchPad.io.portB.dataOut
  // hostRouter.io.mem.dataOut >> scratchPad.io.portB.dataIn
  // decode.io.memPortB >> scratchPad.io.portB.control
  // hostRouter.io.control << decode.io.hostDataFlow

  // hostRouter.io.dram1 <> io.dram1
  // hostRouter.io.dram0 <> io.dram0
}

object Top extends App{
  SpinalSystemVerilog(new Top(SInt(4 bits),Architecture.tiny()))
}