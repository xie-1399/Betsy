package Betsy

import BetsyLibs.SIMCFG
import org.scalatest.funsuite.AnyFunSuite
import spinal.core.sim._
import spinal.core._
import spinal.lib._

/* test 3 paths in the accumulator */

class AccumulatorTest extends AnyFunSuite{

  test("Accumulate It"){

    SIMCFG().compile{
      val dut = new Accumulator(Bits(16 bits),4,512)
      dut
    }.doSimUntilVoid{
      dut =>
        dut.clockDomain.forkStimulus(10)

        //just write
        def write() = {

        }
        //just read from portA
        def read() = {

        }
        // accumulate it
        def accumulate() = {

        }
    }


  }

}
