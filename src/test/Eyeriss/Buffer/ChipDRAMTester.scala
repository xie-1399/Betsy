package Eyeriss.Buffer

import Untils.{AxiInit, DRAMAxiDriver, SIMCFG}
import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.amba4.axi._
import spinal.lib.bus.amba4.axi.sim._
import spinal.lib.bus.misc.SizeMapping

import scala.util.Random
import scala.collection.mutable

class ChipDRAMTester extends AnyFunSuite{

  /* build it using the axi4 bus drive it in the simulation */
  test(" the axi4 drive the DRAM") {

    SIMCFG().compile {
      val dut = new ChipDRAM(DRAMConfig())
      dut
    }.doSimUntilVoid {
      dut =>
        dut.clockDomain.forkStimulus(10)

        dut.io.busPort.ar.valid #= false
        dut.io.busPort.aw.valid #= false
        dut.io.busPort.w.valid #= false
        dut.clockDomain.waitSampling()
        val driver = DRAMAxiDriver

        def test1() = { /* test the simple write */
          for (iter <- 0 until 1000) {
            val randomAddress = Random.nextInt(4096 * 16) * 8
            val data = Random.nextInt(255)
            driver.write(dut.io.busPort, randomAddress, data, dut.clockDomain)
            val ref = dut.DRAM.getBigInt(randomAddress + 7)
            assert(ref == data)
          }
        }

        def test2() = {
          for(iter <- 0 until 1024){
            val randomAddress = Random.nextInt(4096 * 16) * 8
            for(idx <- 0 until 8) {
              dut.DRAM.setBigInt(randomAddress + idx,0)
            }
            dut.clockDomain.waitSampling()
            assert(driver.read(dut.io.busPort, randomAddress, dut.clockDomain) == 0)
          }
        }

        def test3() = {
          var matches = 0
          while(matches <= 1000){
            val randomAddr = Random.nextInt(4096 * 16) * 8
            val data = Random.nextInt(4096 * 16)
            driver.write(dut.io.busPort, randomAddr, data, dut.clockDomain)
            assert(driver.read(dut.io.busPort, randomAddr, dut.clockDomain) == data)
            matches += 1
          }
        }
        test1()
        test2()
        test3()
        simSuccess()
    }
  }

}
