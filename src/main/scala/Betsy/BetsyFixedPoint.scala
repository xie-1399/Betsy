package Betsy

/**
 ** Betsy follow the MiT Licence.(c) xxl, All rights reserved **
 ** Update Time : 2024/4/14      SpinalHDL Version: 1.94       **
 ** You should have received a copy of the MIT License along with this library **
 ** the fixed point datatype is still under tested (wrapper some functions)
 ** all use is based on the SFixed Point instead of the UFixed point **
 */

import Betsy.Until.BetsyModule
import BetsyUntils.SIMCFG
import spinal.core._

object BetsyFixedPoint {

  /* let the SInt value not overflow */
  def saturateFixedPoint(width:Int,x:SInt):SInt = {
    val max = S(1 << (width - 1) - 1)
    val min = S(-1 << (width - 1))
    Mux(x > max,max,Mux(x < min,min,x))
  }

  /**
   * @return m1 * m2 + acc (no care about the overflow and cares simple )
   */
  def rawFixedPointMac(m1: SFix, m2: SFix, acc: SFix): SFix = {
    val mac = m1 * m2 + acc
    mac
  }

  def truncatedFixedPointMac(gen:SFix,m1: SInt, m2: SInt, acc: SInt) = {
    val width = gen.getBitsWidth
    val expNumber = gen.maxExp
    val binaryPoint = gen.bitCount - 1 - expNumber
    val mac = (m1 * m2) +^ (acc << binaryPoint)
    val mask0 = S(1) << (binaryPoint - 1)
    val mask1 = S(1) << (binaryPoint - 1) - 1
    val mask2 = S(1) << binaryPoint

    val adjustment =
      Mux(
        (((mac & mask0) =/= 0) && (((mac & mask1) =/= 0) || ((mac & mask2) =/= 0))),
        S(1),
        S(0)
      )
    val adjusted = (mac >> binaryPoint) + adjustment
    val saturated = saturateFixedPoint(width, adjusted)
    saturated.toSFix.truncated(expNumber exp,binaryPoint exp) /* truncated it */
  }
}


class BetsyFixedPointUsage extends BetsyModule {

  val io = new Bundle{
    val m1 = in(SFix(4 exp,-2 exp))
    val m2 = in(SFix(4 exp,-2 exp))
    val acc = in(SFix(4 exp,-2 exp))
    val mac = out(SFix(4 exp,-2 exp))
  }

  // val zero = BetsyFixedPoint.zero(SFix(4 exp,-2 exp))
  // val one = BetsyFixedPoint.one(SFix(13 exp,-2 exp)) /* extends as 4 value instead */
  // val zero1 = BetsyFixedPoint.zero(Bits(3 bits))
  // println(zero.getBitsWidth.toString) /* all is 7 bits */

  val rawMac = BetsyFixedPoint.truncatedFixedPointMac(io.mac,io.m1.toSInt.resized,io.m2.toSInt.resized,io.acc.resized.toSInt).truncated(4 exp, -2 exp)
  io.mac := rawMac
}


object BetsyFixedPointUsage extends App{
  import spinal.core.sim._
  SIMCFG().compile{
    val dut = new BetsyFixedPointUsage()
    dut
  }.doSimUntilVoid{
    dut =>
      dut.clockDomain.forkStimulus(10)
      dut.io.m1 #= -2
      dut.io.m2 #= 7
      dut.io.acc #= 0.25
      dut.clockDomain.waitSampling()
      println(dut.io.mac.toDouble)
      println(dut.io.mac.maxValue)
      println(dut.io.mac.minValue)
      simSuccess()
  }


}