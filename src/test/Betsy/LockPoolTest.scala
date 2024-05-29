package Betsy

import BetsyLibs.SIMCFG
import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.lib._
import spinal.core.sim._

class LockPoolTest extends AnyFunSuite{

  test("Lock logic"){
    SIMCFG().compile{
      def select[T <: Data](a:T):UInt = U(0)
      val dut = new LockPool(new LockPool(Bits(4 bits),2,1,select))
      dut
    }.doSimUntilVoid{
      dut =>
        dut.clockDomain.forkStimulus(10)


        simSuccess()
    }


  }

}
