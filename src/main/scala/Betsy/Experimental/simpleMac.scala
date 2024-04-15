package Betsy.Experimental

import Betsy.Until._
import spinal.core._

/* simple mac no loading ...*/

class simpleMac[T <: Data with Num[T]](gen:HardType[T]) extends BetsyModule {
  /* mac = acc + m1 * m2*/
  val io = new Bundle{
    val m1 = in(gen())
    val m2 = in(gen())
    val acc = in(gen)
    val mac = out(gen()) /* clip it and keep the data width */
  }

  val macNoclip = io.m1 * io.m2 + io.acc
  io.mac := upDown(macNoclip,gen()).resized
}

object simpleMac extends App{
  SpinalSystemVerilog(new simpleMac(UInt(16 bits)))
}