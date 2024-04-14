package Betsy

import spinal.core._
import scala.util.Random
import BSUntils.SIMCFG

object PipelineTest extends App{
  val pipeline = SpinalSystemVerilog(new Pipeline(Bits(3 bits),10))

  import spinal.core.sim._
  /* wait cycles for the pipeline */
  SIMCFG().compile{
    val dut = new Pipeline(UInt(3 bits),10)
    dut
  }.doSimUntilVoid {
    dut =>
      dut.clockDomain.forkStimulus(10)
      def testCase = 1024
      var fires = 0
      var ends = 0
      val scoresBoard = Array.fill(testCase)(Random.nextInt(8))
      dut.io.input.valid #= false
      dut.clockDomain.waitSampling()

      dut.clockDomain.onSamplings{
        if (dut.io.input.valid.toBoolean && dut.io.input.ready.toBoolean) {
          fires = fires + 1
        }
        dut.io.input.valid.randomize()
        if(fires == testCase) dut.io.input.valid #= false
        dut.io.output.ready.randomize()
        if(fires >= testCase) dut.io.input.payload #= 0
        else dut.io.input.payload #= scoresBoard(fires)
        if(dut.io.output.valid.toBoolean && dut.io.output.ready.toBoolean) {
          assert(dut.io.output.payload.toBigInt == scoresBoard(ends))
          ends += 1
          if(ends == testCase){
            simSuccess()
          }
        }
      }
  }
}

