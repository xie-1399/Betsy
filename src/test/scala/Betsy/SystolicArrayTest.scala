package Betsy

import Betsy.Experimental.Matrix
import BetsyLibs._
import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

class SystolicArrayTest extends AnyFunSuite{

  test("No State Machine"){
    SIMCFG().compile{
      val dut = new SystolicArray(SInt(16 bits),4,4)
      dut.array.mac.foreach(_.foreach(_.weight.simPublic()))
      dut
    }.doSimUntilVoid{
      dut =>

        def VecOperation  = false
        /* init */
        SimTimeout(1 us)
        dut.clockDomain.forkStimulus(10)
        dut.io.control.valid #= false
        dut.io.input.valid #= false
        dut.io.weight.valid #= false
        dut.io.output.ready #= false
        dut.clockDomain.waitSampling(2)
        def testCase = 1024
        def ArrayWidth = 4
        def ArrayHeight = 4
        /* load the weight then */
        val outArray = ArrayBuffer[Array[Int]]()

        def zeroBias() = {
          dut.io.control.valid #= true
          dut.io.control.load #= true
          dut.io.control.zeroes #= true
          dut.io.weight.valid #= false
          dut.io.output.ready.randomize()
          dut.clockDomain.waitSampling()
          dut.io.control.load #= false
          dut.io.control.zeroes #= false
        }
        for(idx <- 0 until testCase){
          var loadWeights = 0
          var loadInputs = 0
          val weights = Array.fill(ArrayWidth) { Array.fill(ArrayHeight) {Random.nextInt(100) - 50}}
          outArray.clear()

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
          if(VecOperation){ /* vector * matrix */
            val inputs = Array.fill(ArrayWidth) {Random.nextInt(100) - 50}
            val refVec = Matrix.vectorMatrixMul(weights, inputs)
            while(loadInputs != 1){
              dut.io.input.payload.zipWithIndex.map { i => i._1 #= inputs.reverse(i._2) }
              dut.io.input.valid.randomize()
              dut.io.output.ready.randomize()
              dut.io.control.valid.randomize()
              dut.clockDomain.waitSampling()
              if(dut.io.control.valid.toBoolean && dut.io.control.ready.toBoolean && dut.io.input.valid.toBoolean && dut.io.input.ready.toBoolean){
                loadInputs += 1
              }
            }
            dut.io.input.valid #= false
            dut.io.control.zeroes #= true
            val macweights = dut.array.mac.map(_.map(_.weight.toInt).toArray).toArray
            val reorder = SimTools.reorderMatrix(macweights)
            val refweight = weights.flatten
            assert(reorder.sameElements(refweight), "load value error !!!")
            dut.clockDomain.waitSamplingWhere(dut.io.output.valid.toBoolean && dut.io.output.ready.toBoolean)
            val out = dut.io.output.payload.map(_.toInt).toArray
            assert(refVec.sameElements(out), "vector * matrix value error ")
          }
          else{ /* matrix * matrix */
            val inputs = Array.fill(ArrayHeight){Array.fill(ArrayWidth) {Random.nextInt(100) - 50 }}
            val refVec = Matrix.multiply(inputs,weights)
            var vecOut = 0
            while (loadInputs < ArrayHeight) {
              dut.io.input.payload.zipWithIndex.map { i => i._1 #= inputs(loadInputs).reverse(i._2) }
              dut.io.input.valid.randomize()
              dut.io.output.ready.randomize()
              dut.io.control.valid.randomize()
              dut.clockDomain.waitSampling()
              if (dut.io.control.valid.toBoolean && dut.io.control.ready.toBoolean && dut.io.input.valid.toBoolean && dut.io.input.ready.toBoolean) {
                loadInputs += 1
              }
              if (dut.io.output.valid.toBoolean && dut.io.output.ready.toBoolean && loadInputs != 0) {
                outArray += dut.io.output.payload.map(_.toInt).toArray
                vecOut += 1
              }
            }
            dut.io.input.valid #= false

            val macweights = dut.array.mac.map(_.map(_.weight.toInt).toArray).toArray
            val reorder = SimTools.reorderMatrix(macweights)
            val refweight = weights.flatten
            assert(reorder.sameElements(refweight), "load value error !!!")

            while(vecOut < ArrayWidth){
              dut.clockDomain.waitSampling()
              if (dut.io.output.valid.toBoolean && dut.io.output.ready.toBoolean && loadInputs != 0) {
                outArray += dut.io.output.payload.map(_.toInt).toArray
                vecOut += 1
              }
            }
            val matrixRef = refVec.flatten
            val matrixOut = outArray.flatten.toArray
            // println(matrixRef.map(_.toLong.toHexString).mkString(","))
            // println(matrixOut.map(_.toLong.toHexString).mkString(","))

            assert(matrixRef.sameElements(matrixOut), "matrix * matrix value error ")
          }

        }
        simSuccess()
    }
  }

  test("State Machine"){

  }
}
