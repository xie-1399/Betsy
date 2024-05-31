package Betsy

import scala.collection.mutable.ArrayBuffer
import spinal.core._
import spinal.core.sim._
import spinal.lib._

import scala.collection.mutable
import scala.collection.mutable.Queue
import scala.util.Random

object SimTools {

  /* with clip */
  def clipValue(width: Int, sign: Boolean, value: Int) = {
    val max = if (sign) Math.pow(2, width - 1) - 1 else Math.pow(2, width) - 1
    val min = if (sign) -Math.pow(2, width - 1) else 0
    val clip = if (value > max) {
      max.toInt
    }
    else if (value < min) {
      min.toInt
    }
    else {
      value
    }
    clip
  }

  def reorderMatrix(array: Array[Array[Int]]) = {
    val rows = array.length
    val cols = array(0).length
    val buffer = new ArrayBuffer[Int]()
    for (j <- cols - 1 to 0 by -1) {
      for (i <- 0 until rows) {
        buffer += array(i)(j)
      }
    }
    buffer.toArray
  }


  def vectorAdd(array1: Array[Int], array2: Array[Int]): Array[Int] = {
    require(array1.length == array2.length, "array size should be equal !!!")
    val result = new Array[Int](array1.length)
    for (i <- array1.indices) {
      result(i) = array1(i) + array2(i)
    }
    result
  }

  def matrixVecAdd(matrix:Array[Array[Int]]):Array[Int] = {
    require(matrix.length >= 2,"vec size should > 2 !!! ")
    val rows = matrix.length
    var result = vectorAdd(matrix(0),matrix(1))
    for(idx <- 2 until rows){
      result = vectorAdd(result,matrix(idx))
    }
    result
  }

  def StreamInit[T <: Data](stream: Stream[T]) = {
    stream.valid #= false
    stream.payload.randomize()
  }

}

/* with a stream queue simulation usage */
class StreamQueue() {
  val queue = new mutable.Queue[BigInt]()
  def push(num: BigInt) = queue.enqueue(num)
  def pop() = queue.dequeue()
  def clear() = queue.clear()
  def length() = queue.size
  def fill(size:Int,init:BigInt = 0,random:Boolean = false) = {
    require(size > 0,"queue fill size should > 0 !!!")
    for(idx <- 0 until size){
     if(random){
       {push(Random.nextInt(1024 * 1024))}
     }else{
       {push(init)}
     }
    }
  }
  def toArray() = {queue.toArray}
}

object StreamQueue {
  def apply[T <: Data]() = new StreamQueue()
}