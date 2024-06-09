package Betsy

import Betsy.Until.BetsyModule
import BetsyLibs._
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

  def writeControl():AccumulatorControl = {
    val w = new AccumulatorControl(arch.accumulatorDepth)
    w.address := io.control.writeAddress
    w.write := True
    w.accumulate := io.control.accumulate
    w
  }

  def readControl():AccumulatorControl = {
    val w = new AccumulatorControl(arch.accumulatorDepth)
    w.address := io.control.readAddress
    w.write := False
    w.accumulate := False
    w
  }

  val accumulator = new Accumulator(gen,simdHeight,arch.accumulatorDepth)
  val aluArray = new ALUArray(gen,arch)

  val aluOutDemux = new BetsyStreamDemux(cloneOf(aluArray.io.outputs.payload),2)
  val accInMux = new BetsyStreamMux(cloneOf(io.inputs.payload),2)
  val accOutDemux = new BetsyStreamDemux(cloneOf(accumulator.io.dataOut),2)

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