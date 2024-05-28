package Betsy

import spinal.core._
import spinal.lib._
import spinal.core.sim._
import Betsy.Until._
import BetsyLibs.SIMCFG
import org.scalatest.funsuite.AnyFunSuite

case class intoBundle() extends Bundle with Address with Size with Stride with Reverse{
  override val address: UInt = UInt(32 bits)
  override val size: UInt = UInt(5 bits)
  override val stride: UInt = UInt(3 bits)
  override val reverse: Bool = Bool()
  val payload:Bits = Bits(16 bits)
}

case class outputBundle() extends Bundle with Address with Size{
  override val address: UInt = UInt(32 bits)
  override val size: UInt = UInt(5 bits)
  val payload:Bits = Bits(16 bits)
}

class StrideHandlerTest extends AnyFunSuite{
  // val rtl = SpinalSystemVerilog(new StrideHandler(inGen = cloneOf(intoBundle()),outGen = cloneOf(outputBundle()),depth = 1024))

  test("stride handler"){
    SIMCFG().compile{
      val dut = new StrideHandler(inGen = cloneOf(intoBundle()),outGen = cloneOf(outputBundle()),depth = 1024)
      dut
    }.doSimUntilVoid{
      dut =>
        dut.clockDomain.forkStimulus(10)

        simSuccess()
    }
  }

}