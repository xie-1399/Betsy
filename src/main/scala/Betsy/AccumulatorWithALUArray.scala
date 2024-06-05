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

class AccumulatorWithALUArray[T <: Data with Num[T]](gen:HardType[T],arch: Architecture) extends BetsyModule{

  def simdHeight = arch.arraySize

  val io = new Bundle{}

  val aluArray = new ALUArray(gen,arch)
  val accumulator = new Accumulator(gen,simdHeight,arch.accumulatorDepth)

  // Todo with the connect logic

}
