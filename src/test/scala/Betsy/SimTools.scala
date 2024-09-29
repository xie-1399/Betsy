package Betsy

import spinal.core._
import spinal.core.sim._
import spinal.lib._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import BetsyLibs.Logger._
import scala.math
object SimTools {

  /* with clip */
  def clipValue(width: Int, sign: Boolean, value: Int): Int = {
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
  // clip the double value
  def clipValueForDouble(max: Double, min: Double, value: Double): Double = {
    val clip = if (value >= max) {
      max
    }
    else if (value < min) {
      min
    }
    else {
      value
    }
    clip
  }
  // ignore the error between Double multiplication and AFix multiplication

  def compareDouble(m1: Double, m2: Double, exp: Int): Boolean = {
    (m1 -  m2).abs < math.pow(2, exp)
  }

  def reorderMatrix(array: Array[Array[Int]], orderChange: Boolean = true): Array[Int] = {
    val rows = array.length
    val cols = array(0).length
    val buffer = new ArrayBuffer[Int]()
    if (orderChange) {
      for (j <- cols - 1 to 0 by -1) {
        for (i <- 0 until rows) {
          buffer += array(i)(j)
        }
      }
    } else {
      for (j <- 0 to cols - 1) {
        for (i <- 0 until rows) {
          buffer += array(i)(j)
        }
      }
    }
    buffer.toArray
  }

  def loadMatrixReorder(test: Array[Array[BigInt]],size:Int): ArrayBuffer[BigInt] = {
    val testArray = new ArrayBuffer[BigInt]()
    val powerSource = math.pow(2,size).toInt
    for (j <- 0 until test(0).length) {
      var temp = BigInt(0)
      for (i <- 0 until test.length) {
        if (i == 0) temp = test(i)(j)
        else temp += math.pow(powerSource, i).toInt * test(i)(j)
      }
      testArray += temp
    }
    testArray
  }

  def signClip(number:BigInt,size:Int):BigInt = {
    val maxValue = math.pow(2,size).toInt
    val result = if(number >= 0) number else maxValue + number
    result
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

  def StreamInit[T <: Data](stream: Stream[T]): Unit = {
    stream.valid #= false
    stream.payload.randomize()
  }

  def genMemoryValue(bit: Int, size: Int, depth: Int): Array[BigInt] = {
    val maxValue = (math.pow(2, bit) / 2 - 1).toInt
    val content = new ArrayBuffer[BigInt]()
    for (idx <- 0 until depth) {
      val values = Array.fill(size) {Random.nextInt(4)}
      val valuesBinary = values.map(value => bigintToBinaryStringWithWidth(value, bit)).reduce(_ + _)
      content += BigInt(valuesBinary, 2)
    }
    content.toArray
  }

  def reshapeMemoryMatrix(binary: String,bitWidth:Int): Array[Array[BigInt]] = {
    require(binary.length % bitWidth == 0, "binary format error!!!")
    val width = binary.length / bitWidth
    val temp = Array.fill(width) {
      Array.fill(1) {
        BigInt(0)
      }
    }
    val binaryArray = binary.grouped(bitWidth).toArray
    val rows = binaryArray.map(BigInt(_, 2))
    for (idx <- 0 until width) {
      temp(idx)(0) = rows(idx)
    }
    temp
  }

  def MemoryContentToMatrix(content:Array[BigInt],size:Int,bit:Int):Array[Array[BigInt]] = {
    val contentBinary = content.map(bigintToBinaryStringWithWidth(_,size * bit))
    val mergedArray = Array.fill(size){Array.fill(size){BigInt(0)}}
    contentBinary.zipWithIndex.foreach{
      binary =>
        val cols = reshapeMemoryMatrix(binary._1,bit)
        for(idx <- 0 until size){
          mergedArray(idx)(size - 1 - binary._2) = cols(size - idx - 1)(0)
        }
    }
    mergedArray
  }

}

/* with a stream queue simulation usage */
class StreamQueue() {
  val queue = new mutable.Queue[BigInt]()
  def push(num: BigInt) = queue.enqueue(num)
  def pop() = queue.dequeue()
  def clear() = queue.clear()
  def length() = queue.size

  def fill(size: Int, init: BigInt = 0, random: Boolean = false): Unit = {
    require(size > 0, "queue fill size should > 0 !!!")
    for (idx <- 0 until size) {
      if (random) {
        {
          push(Random.nextInt(1024 * 1024))
        }
      } else {
        {
          push(init)
        }
      }
    }
  }

  def toArray(): Array[BigInt] = {
    queue.toArray
  }
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

  def noOpGen(arch:Architecture):BigInt = {
    BigInt(0)    /* in fact only highest bit is 0 will be noop*/
  }

  def matMulGen(arch:Architecture,zero:Boolean,localAddress:Int,localStride:Int,
                accumulatorAddress:Int,accumulatorStride:Int,size:Int):(BigInt,String) = {
    val layOut = InstructionLayOut(arch)
    def sizeWidth = layOut.operand2SizeBits
    def localStrideWidth = layOut.stride0SizeBits
    def localAddressWidth = layOut.operand0AddressSizeBits
    def accStrideWidth = layOut.stride1SizeBits
    def accAddressWidth = layOut.operand1AddressSizeBits

    val opcode = 1
    val opBinary = bigintToBinaryStringWithWidth(BigInt(opcode), width = 4)
    val flags = if(zero) 1 else 2
    val flagsBinary = bigintToBinaryStringWithWidth(BigInt(flags), width = 4)

    val localAddressBinary = bigintToBinaryStringWithWidth(BigInt(localAddress), width = localAddressWidth)
    val localStrideBinary = bigintToBinaryStringWithWidth(BigInt(localStride), width = localStrideWidth)

    val accAddressBinary = bigintToBinaryStringWithWidth(BigInt(accumulatorAddress), width = accAddressWidth)
    val accStrideBinary = bigintToBinaryStringWithWidth(BigInt(accumulatorStride), width = accStrideWidth)

    val op0Binary = binaryStringWithWidth(localStrideBinary + localAddressBinary, width = layOut.operand0SizeBits)
    val op1Binary = binaryStringWithWidth(accStrideBinary + accAddressBinary, width = layOut.operand1SizeBits)
    val op2Binary = bigintToBinaryStringWithWidth(BigInt(size), width = sizeWidth)

    val matMulBinary = opBinary + flagsBinary + op2Binary + op1Binary + op0Binary

    (BigInt(matMulBinary,2),matMulBinary)
  }

  def dataMoveGen(arch: Architecture, behaviour: String, localAddress: Int, localStride: Int,
                  accumulatorAddress: Int, accumulatorStride: Int, size: Int): (BigInt, String) = {
    /* the acc address shows DRAM address or accumulator address */
    val layOut = InstructionLayOut(arch)
    def sizeWidth = layOut.operand2SizeBits
    def localStrideWidth = layOut.stride0SizeBits
    def localAddressWidth = layOut.operand0AddressSizeBits
    def accStrideWidth = layOut.stride1SizeBits
    def accAddressWidth = layOut.operand1AddressSizeBits

    val opcode = 2
    var flags = 0
    val opBinary = bigintToBinaryStringWithWidth(BigInt(opcode), width = 4)
    val localAddressBinary = bigintToBinaryStringWithWidth(BigInt(localAddress), width = localAddressWidth)
    val localStrideBinary = bigintToBinaryStringWithWidth(BigInt(localStride), width = localStrideWidth)
    val accAddressBinary = bigintToBinaryStringWithWidth(BigInt(accumulatorAddress), width = accAddressWidth)
    val accStrideBinary = bigintToBinaryStringWithWidth(BigInt(accumulatorStride), width = accStrideWidth)
    val op0Binary = binaryStringWithWidth(localStrideBinary + localAddressBinary, width = layOut.operand0SizeBits)
    val op1Binary = binaryStringWithWidth(accStrideBinary + accAddressBinary, width = layOut.operand1SizeBits)
    val op2Binary = bigintToBinaryStringWithWidth(BigInt(size), width = sizeWidth)
    val dataMoveBinary = behaviour match {
      case "dram0->memory" => {
        println(s"generate dram0 -> memory instruction (from dram0 A:$accumulatorAddress S: $accumulatorStride to local memory A:$localAddress S:$localStride) with size $size")
        flags = 0
        val flagsBinary = bigintToBinaryStringWithWidth(BigInt(flags), width = 4)
        opBinary + flagsBinary + op2Binary + op1Binary + op0Binary
      }
      case "dram1->memory" => {
        println(s"generate dram1 -> memory instruction (from dram1 A:$accumulatorAddress S: $accumulatorStride to local memory A:$localAddress S:$localStride) with size $size")
        flags = 2
        val flagsBinary = bigintToBinaryStringWithWidth(BigInt(flags), width = 4)
        opBinary + flagsBinary + op2Binary + op1Binary + op0Binary
      }
      case "memory->dram0" => {
        println(s"generate memory -> dram0 instruction (from local memory A:$localAddress S:$localStride to dram0 A:$accumulatorAddress S: $accumulatorStride) with size $size")
        flags = 1
        val flagsBinary = bigintToBinaryStringWithWidth(BigInt(flags), width = 4)
        opBinary + flagsBinary + op2Binary + op1Binary + op0Binary
      }
      case "memory->dram1" => {
        println(s"generate memory -> dram1 instruction (from local memory A:$localAddress S:$localStride to dram1 A:$accumulatorAddress S: $accumulatorStride) with size $size")
        flags = 3
        val flagsBinary = bigintToBinaryStringWithWidth(BigInt(flags), width = 4)
        opBinary + flagsBinary + op2Binary + op1Binary + op0Binary
      }
      case "accumulator->memory" => {
        println(s"generate accumulator -> memory instruction (from accumulator A:$accumulatorAddress S: $accumulatorStride to local memory A:$localAddress S:$localStride) with size $size")
        flags = 12
        val flagsBinary = bigintToBinaryStringWithWidth(BigInt(flags), width = 4)
        opBinary + flagsBinary + op2Binary + op1Binary + op0Binary
      }
      case "memory->accumulator" => {
        println(s"generate memory -> accumulator instruction (from local memory A:$localAddress S:$localStride to accumulator A:$accumulatorAddress S: $accumulatorStride) with size $size")
        flags = 13
        val flagsBinary = bigintToBinaryStringWithWidth(BigInt(flags), width = 4)
        opBinary + flagsBinary + op2Binary + op1Binary + op0Binary
      }
      case "memory->accumulator(accumulate)" => {
        println(s"generate memory -> accumulator(accumulate) instruction (from local memory A:$localAddress S:$localStride to accumulator A:$accumulatorAddress S: $accumulatorStride) with size $size")
        flags = 15
        val flagsBinary = bigintToBinaryStringWithWidth(BigInt(flags), width = 4)
        opBinary + flagsBinary + op2Binary + op1Binary + op0Binary
      }
      case _ => {
        throw new Exception("no define local dataflow exception!!!")
      }
    }
    (BigInt(dataMoveBinary, 2), dataMoveBinary)

  }

  def simdGen(arch: Architecture,
              readAddress: Int,
              read:Boolean,
              left:Boolean,
              right:Boolean,
              dest:Boolean,
              op:Int,
              writeAddress: Int,
              write:Boolean,
              accumulate:Boolean): (BigInt, String) = {
    require(op >= 0 & op <= 15, "opcode error...")
    val layOut = InstructionLayOut(arch)
    val opcode = 4
    val flags_0 = read
    val flags_1 = write
    val flags_2 = accumulate
    val simdFlags = bigintToBinaryStringWithWidth(BigInt(op), width = 4) +
      bigintToBinaryStringWithWidth(BigInt(left.toInt), width = 1) + bigintToBinaryStringWithWidth(BigInt(right.toInt), width = 1) +
      bigintToBinaryStringWithWidth(BigInt(dest.toInt), width = 1)

    val flag = flags_0.toInt + (flags_1.toInt * 2) + (flags_2.toInt * 4)
    val opBinary = bigintToBinaryStringWithWidth(BigInt(opcode), width = 4)
    val flagsBinary = bigintToBinaryStringWithWidth(BigInt(flag), width = 4)
    val op0Binary = bigintToBinaryStringWithWidth(BigInt(writeAddress), width = layOut.operand0SizeBits)
    val op1Binary = bigintToBinaryStringWithWidth(BigInt(readAddress), width = layOut.operand1SizeBits)
    val op2Binary = binaryStringWithWidth(simdFlags, width = layOut.operand2SizeBits)
    val simdBinary = opBinary + flagsBinary + op2Binary + op1Binary + op0Binary

    op match {
      case 0 => println(s"Noop (read:$flags_0, write:$flags_1, accumulate:$flags_2), readAddress:$readAddress, writeAddress:$writeAddress, left:$left, right:$right, dest:$dest ")
      case 1 => println(s"Zero (read:$flags_0, write:$flags_1, accumulate:$flags_2), readAddress:$readAddress, writeAddress:$writeAddress, left:$left, right:$right, dest:$dest ")
      case 2 => println(s"Move (read:$flags_0, write:$flags_1, accumulate:$flags_2), readAddress:$readAddress, writeAddress:$writeAddress, left:$left, right:$right, dest:$dest ")
      case 3 => println(s"Not (read:$flags_0, write:$flags_1, accumulate:$flags_2), readAddress:$readAddress, writeAddress:$writeAddress, left:$left, right:$right, dest:$dest ")
      case 4 => println(s"And (read:$flags_0, write:$flags_1, accumulate:$flags_2), readAddress:$readAddress, writeAddress:$writeAddress, left:$left, right:$right, dest:$dest ")
      case 5 => println(s"Or (read:$flags_0, write:$flags_1, accumulate:$flags_2), readAddress:$readAddress, writeAddress:$writeAddress, left:$left, right:$right, dest:$dest ")
      case 6 => println(s"Increment (read:$flags_0, write:$flags_1, accumulate:$flags_2), readAddress:$readAddress, writeAddress:$writeAddress, left:$left, right:$right, dest:$dest ")
      case 7 => println(s"Decrement (read:$flags_0, write:$flags_1, accumulate:$flags_2), readAddress:$readAddress, writeAddress:$writeAddress, left:$left, right:$right, dest:$dest ")
      case 8 => println(s"Add (read:$flags_0, write:$flags_1, accumulate:$flags_2), readAddress:$readAddress, writeAddress:$writeAddress, left:$left, right:$right, dest:$dest ")
      case 9 => println(s"Sub (read:$flags_0, write:$flags_1, accumulate:$flags_2), readAddress:$readAddress, writeAddress:$writeAddress, left:$left, right:$right, dest:$dest ")
      case 10 => println(s"Mul (read:$flags_0, write:$flags_1, accumulate:$flags_2), readAddress:$readAddress, writeAddress:$writeAddress, left:$left, right:$right, dest:$dest ")
      case 11 => println(s"Abs (read:$flags_0, write:$flags_1, accumulate:$flags_2), readAddress:$readAddress, writeAddress:$writeAddress, left:$left, right:$right, dest:$dest ")
      case 12 => println(s"GreaterThan (read:$flags_0, write:$flags_1, accumulate:$flags_2), readAddress:$readAddress, writeAddress:$writeAddress, left:$left, right:$right, dest:$dest ")
      case 13 => println(s"GreaterThanEqual (read:$flags_0, write:$flags_1, accumulate:$flags_2), readAddress:$readAddress, writeAddress:$writeAddress, left:$left, right:$right, dest:$dest ")
      case 14 => println(s"Min (read:$flags_0, write:$flags_1, accumulate:$flags_2), readAddress:$readAddress, writeAddress:$writeAddress, left:$left, right:$right, dest:$dest ")
      case 15 => println(s"Max (read:$flags_0, write:$flags_1, accumulate:$flags_2), readAddress:$readAddress, writeAddress:$writeAddress, left:$left, right:$right, dest:$dest ")
      case _ => throw new Exception("opcode error!!!")
    }
    (BigInt(simdBinary, 2), simdBinary)
  }

  def configureGen(register:Int,value:BigInt,arch: Architecture):BigInt = {
    // pc = 10 / run cycles = 9
    val layOut = InstructionLayOut(arch)
    val opcode = 15
    val flags = 0
    val opBinary = bigintToBinaryStringWithWidth(BigInt(opcode), width = 4)
    val flagsBinary = bigintToBinaryStringWithWidth(BigInt(flags), width = 4)
    val op0Binary = bigintToBinaryStringWithWidth(BigInt(register),width = layOut.operand0SizeBits)
    val op1Binary = bigintToBinaryStringWithWidth(value,width = layOut.operand1SizeBits)
    val op2Binary = bigintToBinaryStringWithWidth(BigInt(0),width = layOut.operand2SizeBits)

    val configureBinary = opBinary + flagsBinary + op2Binary + op1Binary + op0Binary
    val configureInst = BigInt(configureBinary,2)
    configureInst
  }

}