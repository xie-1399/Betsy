package Betsy

import BetsyLibs._
import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._

import scala.util.Random

// Todo with more simd ops like not input...

class ALUArrayTest extends AnyFunSuite{

  test("no Input check") {
    SIMCFG().compile {
      val dut = new ALUArray(SInt(16 bits), Architecture(arraySize = 4))
      dut
    }.doSimUntilVoid {
      dut =>
        def testCase = 10
        dut.clockDomain.forkStimulus(10)
        simSuccess()
    }
  }

  test("vectors add"){
    SIMCFG().compile{
      val dut = new ALUArray(SInt(16 bits),Architecture(arraySize = 4))
      dut
    }.doSimUntilVoid{
      dut =>
        SimTimeout(10 us)
        def testCases = 1024
        var index = 0
        dut.clockDomain.forkStimulus(10)
        dut.io.instruction.valid #= false
        dut.io.inputs.valid #= false
        dut.io.outputs.ready #= false
        dut.clockDomain.waitSampling()

        val simpleAdd = fork {
          while (index != testCases) {
            val left = Array.fill(4) {
              Random.nextInt(1024) - 1024
            }
            val right = Array.fill(4) {
              Random.nextInt(1024) - 1024
            }
            while (!(dut.io.instruction.valid.toBoolean && dut.io.instruction.ready.toBoolean && dut.io.inputs.valid.toBoolean && dut.io.inputs.ready.toBoolean)) {
              dut.io.outputs.ready.randomize()
              dut.io.inputs.valid.randomize()
              dut.io.instruction.valid.randomize()
              dut.io.instruction.payload.op #= 8
              dut.io.instruction.dest #= 1

              dut.io.instruction.sourceLeft #= 0
              dut.io.instruction.sourceRight #= 1
              dut.io.inputs.payload.zipWithIndex.map(i => i._1 #= left(i._2))
              dut.clockDomain.waitSampling()
            }
            dut.io.instruction.valid #= false
            dut.io.inputs.valid #= false
            dut.clockDomain.waitSampling(3) // 2 cycles for the delay and one for the fifo
            assert(dut.io.outputs.valid.toBoolean && dut.io.outputs.ready.toBoolean && dut.io.outputs.payload.map(_.toInt).toArray.sameElements(left))
            while (!(dut.io.instruction.valid.toBoolean && dut.io.instruction.ready.toBoolean && dut.io.inputs.valid.toBoolean && dut.io.inputs.ready.toBoolean)) {
              dut.io.outputs.ready.randomize()
              dut.io.inputs.valid.randomize()
              dut.io.instruction.valid.randomize()
              dut.io.instruction.payload.op #= 8
              dut.io.instruction.dest #= 1

              dut.io.instruction.sourceLeft #= 0
              dut.io.instruction.sourceRight #= 1
              dut.io.inputs.payload.zipWithIndex.map(i => i._1 #= right(i._2))
              dut.clockDomain.waitSampling()
            }
            dut.io.instruction.valid #= false
            dut.io.inputs.valid #= false
            dut.clockDomain.waitSampling(3)
            assert(dut.io.outputs.valid.toBoolean && dut.io.outputs.ready.toBoolean)
            assert(dut.io.outputs.payload.map(_.toInt).toArray.sameElements(SimTools.vectorAdd(left, right)))

            while (!(dut.io.instruction.valid.toBoolean && dut.io.instruction.ready.toBoolean)) { // clear the register
              dut.io.outputs.ready.randomize()
              dut.io.inputs.valid #= false
              dut.io.instruction.valid.randomize()
              dut.io.instruction.payload.op #= 1
              dut.io.instruction.dest #= 1

              dut.io.instruction.sourceLeft #= 0
              dut.io.instruction.sourceRight #= 1
              dut.clockDomain.waitSampling()
            }
            dut.io.instruction.valid #= false
            dut.clockDomain.waitSampling(3)
            assert(dut.io.outputs.valid.toBoolean && dut.io.outputs.ready.toBoolean)
            assert(dut.io.outputs.payload.map(_.toInt).toArray.map(a => a == 0).reduce(_&&_))
            index += 1
          }
          simSuccess()
        }
    }
  }
}
