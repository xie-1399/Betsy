package BetsyLibs

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._

import scala.util.Random

class BetsyStreamTest extends AnyFunSuite {

  test("betsy stream mux"){
    SIMCFG().compile{
     val dut = new BetsyStreamMux[BitVector](Bits(16 bits),4)
     dut
    }.doSimUntilVoid{
      dut =>
        SimTimeout(10 ns)
        dut.clockDomain.forkStimulus(10)
        def testCase = 64
        def num = 4
        dut.io.InStreams.foreach(_.valid #= false)
        dut.io.sel.valid #= false
        dut.clockDomain.waitSampling(2)
        val payloads = Array.fill(num * testCase) {Random.nextInt(4096 * 4)}
        val kinds = Array.fill(testCase){Random.nextInt(num)}
        var idx = 0
        val randomTest = fork{
          while (true){
            dut.io.sel.valid.randomize()
            dut.io.sel.payload #= kinds(idx)
            dut.io.OutStream.ready.randomize()
            for(id <- 0 until num){
              dut.io.InStreams(id).valid.randomize()
              dut.io.InStreams(id).payload #= payloads(id + idx * num)
            }
            dut.clockDomain.waitSampling()
            if(dut.io.OutStream.valid.toBoolean && dut.io.OutStream.ready.toBoolean){
              assert(dut.io.OutStream.payload.toInt == payloads(kinds(idx) + (idx * num)))
              idx += 1
            }
            if(idx == testCase) {simSuccess()}
          }
        }
    }
  }

  test("betsy stream demux") {
    SIMCFG().compile {
      val dut = new BetsyStreamDemux[BitVector](Bits(16 bits), 4)
      dut
    }.doSimUntilVoid {
      dut =>
        dut.clockDomain.forkStimulus(10)
        def testCase = 64
        def num = 4
        dut.io.InStream.valid #= false
        dut.io.sel.valid #= false
        dut.clockDomain.waitSampling(2)
        val payloads = Array.fill(testCase) {Random.nextInt(4096 * 4)}
        val kinds = Array.fill(testCase) {Random.nextInt(num)}
        var idx = 0
        val randomTest = fork {
          while (true) {
            dut.io.sel.valid.randomize()
            dut.io.sel.payload #= kinds(idx)
            dut.io.OutStreams.foreach(_.ready.randomize())
            dut.io.InStream.valid.randomize()
            dut.io.InStream.payload #= payloads(idx)
            dut.clockDomain.waitSampling()

            if (dut.io.InStream.valid.toBoolean && dut.io.InStream.ready.toBoolean) {
              assert(dut.io.OutStreams(kinds(idx)).payload.toInt == payloads(idx))
              assert(dut.io.OutStreams(kinds(idx)).valid.toBoolean && dut.io.OutStreams(kinds(idx)).ready.toBoolean)
              idx += 1
            }
            if (idx == testCase) {
              simSuccess()
            }
          }
        }
    }
  }

}
