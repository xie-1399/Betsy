package Betsy

import spinal.core._
import spinal.core.sim._
import spinal.lib._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import BetsyLibs.Logger._

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

object InstructionGen{

  def loadWeightGen(zero: Boolean, size: Int,
                    stride: Int, address: Int, arch: Architecture): BigInt = {

    val layOut = InstructionLayOut(arch)
    def sizeWidth = layOut.operand1SizeBits
    def strideWidth = layOut.stride0SizeBits
    def addressWidth = layOut.operand0AddressSizeBits

    val opcode = 3
    val opBinary = bigintToBinaryStringWithWidth(BigInt(opcode), width = 4)
    val flags = if (zero) 1 else 0
    val flagsBinary = bigintToBinaryStringWithWidth(BigInt(flags), width = 4)

    val addressBinary = bigintToBinaryStringWithWidth(BigInt(address), width = addressWidth)
    val strideBinary = bigintToBinaryStringWithWidth(BigInt(stride), width = strideWidth)
    val op0Binary = binaryStringWithWidth(strideBinary + addressBinary, width = layOut.operand0SizeBits)
    val op1Binary = bigintToBinaryStringWithWidth(BigInt(size), width = sizeWidth)
    val op2Binary = bigintToBinaryStringWithWidth(BigInt(0), width = layOut.operand2SizeBits)

    val LoadWeightBinary = opBinary + flagsBinary + op2Binary + op1Binary + op0Binary
    val loadInst = BigInt(LoadWeightBinary, 2)
    loadInst
  }

}

//object test extends App{
//  println(InstructionGen.loadWeightGen(true,16,4,128,Architecture.tiny()))
//  println(InstructionGen.loadWeightGen(false,16,4,128,Architecture.tiny()))
//}