package Betsy

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._
import BetsyLibs._

// test all kinds of models like resnet/yolo in lots of systolic array config
// first get the all instruction


class TopSim extends AnyFunSuite{

  val arch = Architecture.normal()
  val instructionFile = ""

  test("top") {
    SIMCFG().compile {
      val dut = new Top(SInt(16 bits), arch = arch) // 64 * 64 and 16 bits
      dut
    }.doSimUntilVoid {
      dut =>
        dut.clockDomain.forkStimulus(10)




        simSuccess()
    }
  }

}
