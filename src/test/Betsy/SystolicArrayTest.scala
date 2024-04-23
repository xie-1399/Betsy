package Betsy

import org.scalatest.funsuite.AnyFunSuite
import spinal.core.sim._
import spinal.core._
import BetsyLibs._
import Betsy._

import scala.util.Random

class SystolicArrayTest extends AnyFunSuite{

  test("No State Machine"){
    SIMCFG().compile{
      val dut = new SystolicArray(SInt(16 bits),4,4)
      dut.array.mac.foreach(_.foreach(_.weight.simPublic()))
      dut
    }.doSimUntilVoid{
      dut =>
        /* init */
        SimTimeout(10 ns)
        dut.clockDomain.forkStimulus(10)
        dut.io.control.valid #= false
        dut.io.input.valid #= false
        dut.io.weight.valid #= false
        dut.io.output.ready #= false
        dut.clockDomain.waitSampling(2)
        def testCase = 16
        def ArrayWidth = 4
        def ArrayHeight = 4
        /* load the weight then */

        def zeroBias() = {
          dut.io.control.valid #= true
          dut.io.control.load #= true
          dut.io.control.zeroes #= true
          dut.io.weight.valid #= false
          dut.io.output.ready.randomize()
          dut.clockDomain.waitSampling(2) // Todo why not 2 cycles
          dut.io.control.load #= false
          dut.io.control.zeroes #= false
        }


        for(idx <- 0 until testCase){
          var loadWeights = 0
          val weights = Array.fill(ArrayWidth) { Array.fill(ArrayHeight) {Random.nextInt(100) - 50}}
          while(loadWeights != ArrayWidth){
            dut.io.control.valid.randomize()
            dut.io.control.load.randomize()
            dut.io.output.ready.randomize()
            dut.io.control.zeroes #= false
            dut.io.weight.valid.randomize()
            dut.io.weight.payload.zipWithIndex.foreach(w => w._1 #= weights(loadWeights)(w._2))
            dut.clockDomain.waitSampling()
            if(dut.io.control.valid.toBoolean && dut.io.control.ready.toBoolean && dut.io.weight.valid.toBoolean && dut.io.weight.ready.toBoolean){
              loadWeights += 1
            }
          }
          zeroBias()
          val macweights = dut.array.mac.map(_.map(_.weight.toInt).toArray).toArray
          val reorder = SimTools.reorderMatrix(macweights)
          val refweight = weights.flatten
          assert(reorder.sameElements(refweight), "load value error !!!")
        }
        simSuccess()
    }

  }


  test("State Machine"){

  }
}
