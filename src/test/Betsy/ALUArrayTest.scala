package Betsy

import org.scalatest.funsuite.AnyFunSuite
import spinal.core.sim._
import spinal.core._
import BetsyLibs._
import spinal.core

import scala.util.Random

// Todo with more simd ops

class ALUArrayTest extends AnyFunSuite{

  test("vectors add"){
    SIMCFG().compile{
      val dut = new ALUArray(SInt(16 bits),Architecture(arraySize = 4))
      dut
    }.doSimUntilVoid{
      dut =>
        def testCase = 2
        var index = 0
        dut.clockDomain.forkStimulus(10)
        dut.io.instruction.valid #= false
        dut.io.inputs.valid #= false
        dut.io.outputs.ready #= false
        dut.clockDomain.waitSampling()
        val simpleAdd = fork {
          val left = Array.fill(4) {
            Random.nextInt(1024) - 1024
          }
          val right = Array.fill(4) {
            Random.nextInt(1024) - 1024
          }

          dut.io.outputs.ready #= true
          dut.io.inputs.valid #= true
          dut.io.instruction.valid #= true
          dut.io.instruction.payload.op #= 8
          dut.io.instruction.dest #= 1
          dut.io.instruction.sourceLeft #= 0
          dut.io.instruction.sourceRight #= 1
          dut.io.inputs.payload.zipWithIndex.map(i => i._1 #= left(i._2))
          dut.clockDomain.waitSampling()
          dut.io.inputs.payload.zipWithIndex.map(i => i._1 #= right(i._2))
          dut.clockDomain.waitSampling()

          dut.io.instruction.valid #= false
          dut.io.inputs.valid #= false
          dut.clockDomain.waitSampling(3)
          val res = SimTools.vectorAdd(left,right)
          println("ref value:" + res.mkString(","))
          dut.io.outputs.payload.map(_.toInt).toArray.foreach(print)
          simSuccess()
        }

//        val Add = fork{
//          while(index != testCase) {
//            val inputs = adds(index)
//            dut.io.instruction.valid.randomize()
//            dut.io.inputs.valid.randomize()
//            dut.io.outputs.ready #= true
//            dut.io.instruction.payload.op #= 8
//            dut.io.instruction.dest #= 1
//            dut.io.instruction.sourceLeft #= 0
//            dut.io.instruction.sourceRight #= 1
//            dut.io.inputs.payload.zipWithIndex.map(i => i._1 #= inputs(i._2))
//            dut.clockDomain.waitSampling()
//            if(dut.io.instruction.valid.toBoolean && dut.io.instruction.ready.toBoolean && dut.io.inputs.valid.toBoolean && dut.io.inputs.ready.toBoolean){
//              index += 1
//            }
//            if(dut.io.outputs.valid.toBoolean && dut.io.outputs.ready.toBoolean){
//              addNum += 1
//            }
//          }
//          while (addNum != testCase){
//            dut.io.instruction.valid #= false
//            dut.clockDomain.waitSampling()
//            if (dut.io.outputs.valid.toBoolean && dut.io.outputs.ready.toBoolean) {
//              addNum += 1
//            }
//          }
//          dut.clockDomain.waitSampling(10)
//          val ref = SimTools.matrixVecAdd(adds)
//          ref.foreach(print)
//          println()
//          dut.io.outputs.payload.map(_.toInt).toArray.foreach(print)
//          simSuccess()
//        }

    }
  }
}
