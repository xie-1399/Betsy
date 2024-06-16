package Betsy.Experimental

import spinal.core._
import spinal.core.sim._
import BetsyLibs._
import scala.util.Random
import Betsy.Matrix


object TPUArrayTest extends App{
  /* the data order is hard to control */
  import spinal.core.sim._
  SIMCFG(gtkFirst = true).compile{
    val dut = new TPUArray(UInt(8 bits),UInt(16 bits))
    dut
  }.doSimUntilVoid{
    dut =>
      dut.clockDomain.forkStimulus(10)
      def height = 3
      def width = 3
      def cycles = 8
      val weight = Array.fill(height * width) {
        Random.nextInt(100)
      }
      val activate = Array.fill(height * width) {
        Random.nextInt(100)
      }

      val weightArray = weight.grouped(height).toArray
      val activateArray = activate.grouped(width).toArray

      val diagweight = Matrix.diagArrange(weightArray, false)
      val diagactivate = Matrix.diagArrange(activateArray, true)

      val refMatrix = Matrix.multiply(weightArray, activateArray)
      Matrix.printMatrix(refMatrix)

      val weightArrays = Matrix.TPUreshape(diagweight, height, 8)
      val activateArrays = Matrix.TPUreshape(diagactivate, width, 8)

      for (idx <- 0 until cycles) {
        dut.io.weight.zipWithIndex.foreach { w => w._1 #= weightArrays(idx)(w._2) }
        dut.io.activation.zipWithIndex.foreach { a => a._1 #= activateArrays(idx)(a._2) }
        dut.clockDomain.waitSampling()
      }
      val ref = refMatrix.flatten
      val acc = dut.io.accSum.map(_.toInt).toArray

      dut.io.accSum.foreach {
        a =>
          print(a.toBigInt + " ")
      }
      println()
      assert(ref.sameElements(acc), "matrix mul value error !!!")
      simSuccess()
  }
}
