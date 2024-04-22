package Betsy

import org.scalatest.funsuite.AnyFunSuite
import spinal.core.sim._
import spinal.core._
import BetsyLibs._
import spinal.core

import scala.util.Random

class ALUArrayTest extends AnyFunSuite{

  test("4 vectors add"){
    SIMCFG().compile{
      val dut = new ALUArray(SInt(8 bits),Architecture())
      dut
    }.doSimUntilVoid{
      dut =>
        def testCase = 1024
        var index = 0
        dut.clockDomain.forkStimulus(10)
        dut.io.instruction.valid #= false
        dut.io.inputs.valid #= false
        dut.io.outputs.ready #= false
        dut.clockDomain.waitSampling()
        val adds = Array.fill(testCase){Array.fill(4){Random.nextInt(100) - 100}}

        val Add = fork{
          while(index != testCase) {
            for (idx <- 0 until 4) {
              dut.io.instruction.valid #= true
              dut.io.inputs.valid #= true
              dut.io.inputs.payload.foreach(_ #= adds(index)(idx))
              dut.io.outputs.ready #= true
              dut.io.instruction.payload.op #= 8
              dut.io.instruction.dest #= 1
              dut.io.instruction.sourceLeft #= 0
              dut.io.instruction.sourceRight #= 1
              dut.clockDomain.waitSampling()
              index += 1

              dut.io.instruction.valid #= false
              dut.io.inputs.valid #= false
              dut.clockDomain.waitSamplingWhere(dut.io.outputs.valid.toBoolean)
            }
          }
          simSuccess()
        }
    }


  }

}
