package Betsy

import spinal.core._
import spinal.lib._
import BetsyLibs._
import Betsy.Until._

/**
 ** Betsy follow the MiT Licence.(c) xxl, All rights reserved **
 ** Update Time : 2024/5/31      SpinalHDL Version: 1.94       **
 ** You should have received a copy of the MIT License along with this library **
 ** integration the compute/control/datamove module in the BetsyNPU **
 ** The ML activations, average and maximum pooling, normalization, and image resizing use SIMD instruction.
 ** Some ML operations, such as padding, are achieved by changing the memory layout. **
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
  val scratchPad = new DualPortMem(Vec(gen,arch.arraySize),arch.localDepth,initContent = initContent) // no mask with
  val systolicArray = new SystolicArray(gen,height = arch.arraySize,width = arch.arraySize)
  val accumulatorWithALUArray = new AccumulatorWithALUArray(gen,arch)
  val localRouter = new LocalRouter(Vec(gen,arch.arraySize),arch)

  def decode_to_localRouter():Unit = {
    localRouter.io.control << decode.io.localDataFlow
  }

  def localRouter_to_accumulatorALU():Unit = {
    localRouter.io.accumulatorDataFlow.accIn >> accumulatorWithALUArray.io.inputs
    localRouter.io.accumulatorDataFlow.accOut << accumulatorWithALUArray.io.outputs
  }

  def localRouter_to_systolicArray():Unit ={
    systolicArray.io.weight << localRouter.io.arrayDataFlow.weight
    systolicArray.io.input << localRouter.io.arrayDataFlow.input
    systolicArray.io.output >> localRouter.io.arrayDataFlow.output
  }

  def localRouter_to_scratchPad():Unit = {
    localRouter.io.memoryDataFlow.memOut << scratchPad.io.portA.dataOut
    localRouter.io.memoryDataFlow.memIn >> scratchPad.io.portA.dataIn
  }

  def decode_to_systolicArray():Unit = {
    systolicArray.io.control << decode.io.systolicArrayControl
  }

  def decode_to_accumulatorALU():Unit = {
    decode.io.accumulatorWithALUArrayControl >> accumulatorWithALUArray.io.control
  }

  def decode_to_scratchPad():Unit = {
    scratchPad.io.portA.control << decode.io.memPortA
    scratchPad.io.portB.blockPort()

  }

  def ConnectAll(): Unit = {
    decode_to_localRouter()
    localRouter_to_accumulatorALU()
    localRouter_to_systolicArray()
    localRouter_to_scratchPad()
    decode_to_systolicArray()
    decode_to_accumulatorALU()
    decode_to_scratchPad()
    decode.io.instruction.payload := InstructionFormat.fromBits(io.instruction.payload)(instructionLayOut)
    decode.io.instruction.arbitrationFrom(io.instruction)
  }

  ConnectAll() //connect the top module

}

object Top extends App{
  SpinalSystemVerilog(new Top(SInt(4 bits),Architecture.tiny()))
}