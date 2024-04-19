package Betsy

import Betsy.Experimental.Matrix
import BetsyLibs.SIMCFG
import org.scalatest.funsuite.AnyFunSuite
import spinal.core
import spinal.core.sim._
import spinal.core._

import scala.util.Random
import scala.collection.mutable.ArrayBuffer

class InnerSystolicArrayTest extends AnyFunSuite{

  /*  (notice the difference between def and val)*/
  val ArrayHeight =  Random.nextInt(16) + 1
  val ArrayWidth = Random.nextInt(16) + 1
  val testCase = 1024

  def init(dut:InnerSystolicArray[SInt]) = {
    dut.clockDomain.forkStimulus(10)
    dut.io.load #= false
    dut.io.input.foreach(_ #= 0)
    dut.io.weight.foreach(_ #= 0)
    dut.clockDomain.waitSampling(2)
  }

  def reorderMatrix(array:Array[Array[Int]]) = {
    val rows = array.length
    val cols = array(0).length
    val buffer = new ArrayBuffer[Int]()
    for( j <- cols - 1 to 0 by -1 ){
      for( i <- 0 until rows ){
        buffer += array(i)(j)
      }
    }
    buffer.toArray
  }

  def loadWeight(dut:InnerSystolicArray[SInt],weights:Array[Array[Int]]) = {
    dut.io.load #= true
    dut.io.input.randomize()
    for(i <- 0 until ArrayWidth){
      for( j <- 0 until ArrayHeight){
        dut.io.weight(j) #= weights(i)(j)
      }
      dut.clockDomain.waitSampling()
    }
    dut.io.weight.foreach(_ #= 0)
    dut.clockDomain.waitSampling()
    dut.io.load #= false // load finish
  }

  test("vector * matrix") {
    SIMCFG().compile {
      val dut = new InnerSystolicArray(SInt(16 bits), ArrayHeight, ArrayWidth)
      dut.mac.foreach(_.foreach(_.weight.simPublic())) /* for assert purpose */
      dut
    }.doSimUntilVoid {
      dut
      =>
        println(s"******* Array size : $ArrayHeight × $ArrayWidth *******")
        init(dut)
        for( idx <- 0 until testCase){
          val weights = Array.fill(ArrayWidth) {
            Array.fill(ArrayHeight) {
              Random.nextInt(100) - 50
            }
          }
          loadWeight(dut, weights = weights)
          val inputs = Array.fill(ArrayWidth) {
            Random.nextInt(100) - 50
          }
          val refVec = Matrix.vectorMatrixMul(weights, inputs)
          dut.io.input.zipWithIndex.map { i => i._1 #= inputs.reverse(i._2) }
          dut.clockDomain.waitSampling()
          dut.io.input.foreach(_ #= 0)
          val macweights = dut.mac.map(_.map(_.weight.toInt).toArray).toArray
          /* mac weights */
          val reorder = reorderMatrix(macweights)
          val refweight = weights.flatten
          assert(reorder.sameElements(refweight), "load value error !!!")
          dut.clockDomain.waitSampling(ArrayWidth + ArrayHeight - 1)
          val out = dut.io.output.map(_.toInt).toArray
          assert(refVec.sameElements(out), "vector * matrix value error ")
        }
        simSuccess()
    }
  }

  test("matrix * matrix"){
    SIMCFG().compile{
      val dut = new InnerSystolicArray(SInt(16 bits),ArrayHeight,ArrayWidth)
      dut.mac.foreach(_.foreach(_.weight.simPublic()))
      dut
    }.doSimUntilVoid{
      dut =>
        println(s"******* Array size : $ArrayHeight × $ArrayWidth *******")
        init(dut)
        val outArray = ArrayBuffer[Array[Int]]()
        for (idx <- 0 until testCase) {
          val weights = Array.fill(ArrayWidth) {
            Array.fill(ArrayHeight) {
              Random.nextInt(100) - 50
            }
          }
          outArray.clear()
          loadWeight(dut, weights = weights)
          val inputs = Array.fill(ArrayHeight){Array.fill(ArrayWidth) {Random.nextInt(100) - 50 }} /* */
          val refVec = Matrix.multiply(inputs,weights)
          for(idx <- 0 until ArrayHeight){
            dut.io.input.zipWithIndex.map { i => i._1 #= inputs(idx).reverse(i._2) }
            dut.clockDomain.waitSampling()
          }
          dut.io.input.foreach(_ #= 0)
          val macweights = dut.mac.map(_.map(_.weight.toInt).toArray).toArray
          /* mac weights */
          val reorder = reorderMatrix(macweights)
          val refweight = weights.flatten
          assert(reorder.sameElements(refweight), "load value error !!!")
          dut.clockDomain.waitSampling(ArrayWidth)
          for(i <- 0 until ArrayHeight){
            outArray += dut.io.output.map(_.toInt).toArray
            dut.clockDomain.waitSampling()
          }
          val matrixRef = refVec.flatten
          val matrixOut = outArray.flatten.toArray
          assert(matrixRef.sameElements(matrixOut), "matrix * matrix value error ")
        }
        simSuccess()
    }
  }

}
