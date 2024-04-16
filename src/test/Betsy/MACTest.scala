package Betsy


import BetsyLibs.SIMCFG
import spinal.core.sim._
import org.scalatest.funsuite.AnyFunSuite
import spinal.core
import spinal.core._
import scala.util.Random

// Todo add fixed point Mac
class MACTest extends AnyFunSuite {

  /* with clip */
  def clipValue(width: Int, sign: Boolean, value: Int) = {
    val max = if (sign) Math.pow(2, width - 1) - 1 else Math.pow(2, width) - 1
    val min = if (sign) -Math.pow(2, width - 1) else 0
    val clip = if (value > max) {
      max.toInt
    }
    else if (value < min) {
      min.toInt
    }
    else {
      value
    }
    clip
  }

  test("SINT Clip"){
    SIMCFG().compile {
      val dut = new MAC(SInt(8 bits))
      dut
    }.doSimUntilVoid {
      dut =>
        dut.clockDomain.forkStimulus(10)
        def width = 8
        def sign = true
        def testCase = 1024
        val weight = Array.fill(testCase) {
          Random.nextInt(100) - 50
        }
        val adds = Array.fill(testCase) {
          Random.nextInt(100) - 50
        }
        val activation = Array.fill(testCase) {
          Random.nextInt(100) - 50
        }
        dut.io.load #= false
        dut.io.mulInput #= 0
        dut.io.addInput #= 0
        var idx = 0
        dut.clockDomain.waitSampling()
        val Mac = fork {
          // first load and then
          while (true) {
            dut.io.load #= true
            dut.io.addInput #= weight(idx)
            dut.clockDomain.waitSampling()

            dut.io.load #= false
            dut.io.mulInput #= activation(idx)
            dut.io.addInput #= adds(idx)
            dut.clockDomain.waitSampling(2)
            val refMac = weight(idx) * activation(idx) + adds(idx)
            assert(dut.io.macOut.toBigInt == clipValue(width, sign, refMac), s"${dut.io.macOut.toBigInt} is not match ${clipValue(width, sign, refMac)}")
            idx += 1
            if (idx == testCase) {
              simSuccess()
            }
          }
        }
    }
  }


  test("UINT Clip"){
    SIMCFG().compile {
      val dut = new MAC(UInt(16 bits))
      dut
    }.doSimUntilVoid {
      dut =>
        dut.clockDomain.forkStimulus(10)
        def width = 16
        def sign = false
        def testCase = 1024

        val weight = Array.fill(testCase) {
          Random.nextInt(1024 * 8)
        }
        val adds = Array.fill(testCase) {
          Random.nextInt(1024 * 8)
        }
        val activation = Array.fill(testCase) {
          Random.nextInt(1024 * 8)
        }
        dut.io.load #= false
        dut.io.mulInput #= 0
        dut.io.addInput #= 0
        var idx = 0
        dut.clockDomain.waitSampling()
        val Mac = fork {
          // first load and then
          while (true) {
            dut.io.load #= true
            dut.io.addInput #= weight(idx)
            dut.clockDomain.waitSampling()

            dut.io.load #= false
            dut.io.mulInput #= activation(idx)
            dut.io.addInput #= adds(idx)
            dut.clockDomain.waitSampling(2)
            val refMac = weight(idx) * activation(idx) + adds(idx)
            assert(dut.io.macOut.toBigInt == clipValue(width, sign, refMac), s"${dut.io.macOut.toBigInt} is not match ${clipValue(width, sign, refMac)}")
            idx += 1
            if (idx == testCase) {
              simSuccess()
            }
          }
        }
    }
  }

}
