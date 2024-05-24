package Betsy

import Betsy.Until._
import spinal.core._
import spinal.lib._
import BetsyLibs._

class Accumulator[T<:Data with Num[T]](gen:T,SimdWidth:Int,depth:Int) extends BetsyModule{

  val io = new Bundle{
    val dataIn = slave(Stream(Vec(gen,SimdWidth)))
    val dataOut = master(Stream(Vec(gen,SimdWidth)))
    val control = slave(Stream(AccumulatorControl(depth)))
  }

  val accumulator = new DualPortMem(gen,depth)

  /* two ports accumulator port A write and port B read*/

  val portA = accumulator.io.portA
  val portB = accumulator.io.portB

  /* Port A*/
  // io.dataOut <> portA.dataOut

}
