package Betsy.Experimental

import Betsy.Until._
import spinal.core._
import spinal.lib._

/**
 ** Sloan follow the MiT Licence.(c) xxl, All rights reserved **
 ** Update Time : 2024/4/15      SpinalHDL Version: 1.94       **
 ** You should have received a copy of the MIT License along with this library **
 ** TPU experimental mac calculate PE in the TPU Array **
 */

class TPUMac[T <: Data with Num[T]](gen:HardType[T],accType:HardType[T]) extends BetsyModule {
  val io = new Bundle{
    val m1 = in(gen())
    val m2 = in(gen())
    val m1pass = out(gen())
    val m2pass = out(gen())
    val psum = out(accType())
  }

  /* each PE */
  if(accType.getBitsWidth != 2 * gen.getBitsWidth) {SpinalWarning("the width may be lose value !!! ")}
  val m1pass = RegNext(io.m1).init(zero(gen()))
  val m2pass = RegNext(io.m2).init(zero(gen()))

  io.m1pass := m1pass
  io.m2pass := m2pass
  val psum = Reg(accType).init(zero(accType()))

  val macvalue = io.m1 * io.m2
  psum := (macvalue + psum).resized
  io.psum := psum.resized
}



object TPUMac extends App{
  SpinalSystemVerilog(new TPUMac(UInt(16 bits),UInt(32 bits)))
}