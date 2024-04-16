package Betsy.Experimental

import Betsy.Until._
import BetsyLibs.SIMCFG
import spinal.core._

import scala.util.Random

/**
 ** Sloan follow the MiT Licence.(c) xxl, All rights reserved **
 ** Update Time : 2024/4/15      SpinalHDL Version: 1.94       **
 ** You should have received a copy of the MIT License along with this library **
 ** TPU experimental Array with data control Todo tested !!! **
 */


class TPUArray[T <: Data with Num[T]](gen:HardType[T],accType:HardType[T],height:Int = 3,width:Int = 3) extends BetsyModule{
  val io = new Bundle{
    val weight = in Vec(gen,height)
    val activation = in Vec(gen,width)

    val accSum = out Vec(accType(),height * width)
  }

   val PEs = for(i <- 0 until height)
     yield for(j <- 0 until width) yield {new TPUMac(gen(),accType())}

  /* connect the PEs with TPU-like */

  /* first row*/
  PEs(0)(0).io.m1 := io.weight(0)
  PEs(0)(0).io.m2 := io.activation(0)

  for(j <- 1 until width){
    PEs(0)(j).io.m2 := io.activation(j)
    PEs(0)(j).io.m1 := PEs(0)(j-1).io.m1pass
  }

  /* first column */
  for(i <- 1 until height){
    PEs(i)(0).io.m1 := io.weight(i)
    PEs(i)(0).io.m2 := PEs(i - 1)(0).io.m2pass
  }

  /* connect the body like */
  for( i <- 1 until height){
    for( j <- 1 until width){
      PEs(i)(j).io.m1 := PEs(i)(j-1).io.m1pass
      PEs(i)(j).io.m2 := PEs(i-1)(j).io.m2pass
    }
  }

  val sums = PEs.flatten.map(_.io.psum)
  io.accSum.zipWithIndex.map{
    acc =>
      acc._1 := sums(acc._2)
  }
}


object TPUArray extends App{

  import spinal.core.sim._
  import scala.collection.mutable.ArrayBuffer
  SIMCFG().compile{
    val dut = new TPUArray(UInt(8 bits),UInt(16 bits))
    dut
  }.doSimUntilVoid{
    dut =>
      dut.clockDomain.forkStimulus(10)

      /* also fill with zero */
      def mkArray(size:Int,cycles:Int) = {
        val array = Array.fill(cycles)(0)
        var arrayIndex = 0
        val remain = cycles - (2 * size - 1)
        require(remain >= 0)
        for(idx <- 1 to size){
          array(arrayIndex) = idx
          arrayIndex += 1
        }
        for(idx <- size - 1 until 0 by -1){
          array(arrayIndex) = idx
          arrayIndex += 1
        }
        for(idx <- 0 until remain){
          array(arrayIndex) = 0
          arrayIndex += 1
        }
        array
      }

      /* reshape the data format to the Array */
      def reshape(raw:Array[Int],size:Int,cycles:Int) = {
        require(raw.length == size * size)
        require(cycles >= size * 2 + 1)
        val bufferArray = new ArrayBuffer[Array[Int]]()
        var continue = 0
        val numArray = mkArray(size, cycles)

        for(idx <- 0 until cycles){
          val array = Array.fill(size){0}
          if(continue == raw.length){
            bufferArray += array
          }else{
            val num = numArray(idx)
            if(idx < size){
              for(index <- 0 until num){
                array(index) = raw(continue + index)
              }
            }else{
              for (index <- 0 until num) {
                array(size + index - num) = raw(continue + index)
              }
            }
            continue += num
            bufferArray += array
          }
        }
        bufferArray
      }

      // mkArray(size = 4,cycles = 10).foreach(println)
      // val arrays = reshape(Array(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16),size = 4,cycles = 19)
      // for(idx <- 0 until arrays.length){
      //arrays(idx).foreach(print)
      //println()
      //}

      def height = 3
      def width = 3
      def cycles = 8
      // val weight = Array.fill(height * width){Random.nextInt(100)}
      // val activate = Array.fill(height * width){Random.nextInt(100)}
      val weight = Array(1,2,3,4,5,6,7,8,9)
      val activate = Array(1,2,3,4,5,6,7,8,9)

      val weightArray = weight.grouped(height).toArray
      val activateArray = activate.grouped(width).toArray

      // Todo add diag function
      val diagweight = Array(1,2,4,3,5,7,6,8,9)
      val diagactivate = Array(1,4,2,7,5,3,8,6,9)

      val refMatrix = Matrix.multiply(weightArray,activateArray)
      Matrix.printMatrix(refMatrix)

      val weightArrays = reshape(diagweight,height,8)
      val activateArrays = reshape(diagactivate,width,8)

      for(idx <- 0 until cycles){
        dut.io.weight.zipWithIndex.foreach{ w => w._1 #= weightArrays(idx)(w._2)}
        dut.io.activation.zipWithIndex.foreach{ a => a._1 #= activateArrays(idx)(a._2)}
        dut.clockDomain.waitSampling()
      }

      dut.io.accSum.foreach{
        a =>
          print(a.toBigInt + " ")
      }
      simSuccess()

  }

}
