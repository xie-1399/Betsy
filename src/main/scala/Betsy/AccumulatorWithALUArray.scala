package Betsy

import Betsy.Until.BetsyModule
import spinal.core._
import spinal.lib._

/**
 ** Betsy follow the MiT Licence.(c) xxl, All rights reserved **
 ** Update Time : 2024/6/6      SpinalHDL Version: 1.94       **
 ** You should have received a copy of the MIT License along with this library **
 ** combine the alu array and teh accumulator with control **
 */

class AccumulatorWithALUArray[T <: Data with Num[T]](gen:HardType[T],arch: Architecture)(implicit layOut: InstructionLayOut) extends BetsyModule{

  def simdHeight = arch.arraySize

  val io = new Bundle{
    val inputs = slave(Stream(Vec(gen,simdHeight)))
    val outputs = master(Stream(Vec(gen,simdHeight)))
    val control = slave(Stream(AccumulatorWithALUArrayControl(layOut)))
  }

  val aluArray = new ALUArray(gen,arch)
  val accumulator = new Accumulator(gen,simdHeight,arch.accumulatorDepth)

  //Todo with the accumulate

  // alu array
  aluArray.io.outputs >> accumulator.io.dataIn
  aluArray.io.inputs << io.inputs
  aluArray.io.instruction.payload <> io.control.SIMDInstruction
  aluArray.io.instruction.arbitrationFrom(io.control)


  //accumulator

  accumulator.io.control.payload.accumulate := io.control.payload.accumulate
  accumulator.io.control.payload.write := io.control.payload.write

}
