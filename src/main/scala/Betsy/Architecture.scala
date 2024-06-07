package Betsy


/**
 ** Betsy follow the MiT Licence.(c) xxl, All rights reserved **
 ** Update Time : 2024/4/21      SpinalHDL Version: 1.94       **
 ** You should have received a copy of the MIT License along with this library **
 ** the config and arch about the Betsy (the arch decides the Instruction Layout) **
 */

import spinal.core._
import spinal.lib._

abstract class ArchitectureDataType(name:String){
  def sizeBytes:Double
  override def toString: String = name
}

case class Architecture( dataType:String = "UInt_16",
                         arraySize:Int = 16, /* the default array size is 16 * 16 */
                         dram0Depth: Long = 1024 * 1024,
                         dram1Depth: Long = 1024 * 1024,
                         localDepth: Long = 2048,  /* control the local router move data size */
                         accumulatorDepth:Long = 512,
                         simdRegistersDepth:Int = 1,
                         stride0Depth: Int = 1,
                         stride1Depth: Int = 1,
                         pcWidth:Int = 32,   /* the program counter width */
                         numberOfThreads:Int = 1
                       ) extends ArchitectureDataType(dataType) {
  val dataWidth = dataType.split("_")(1).toInt
  require(dataWidth > 0, "the data width must > 0 !!!")
  val dataKind = dataType.split("_")(0)
  require(dataKind == "SInt" || dataKind == "UInt", "Only Support (SInt,UInt) datatype, Please check the name!!!")

  override def sizeBytes: Double = dataWidth / 8
}

/* add more architecture examples here */

/* 64 bits tiny instruction for the betsy npu */

object Architecture{

  def tiny(): Architecture = {
    // 4 + 4 + 16 + 20 + 20 = 64
    val arch = Architecture(
      dataType = "SInt_4",
      arraySize = 8,
      dram0Depth = 1024 * 1024,  // 20 bits dram address
      dram1Depth = 1024 * 1024,
      localDepth = 8192, // 13 bits
      accumulatorDepth = 2048,
      simdRegistersDepth = 1,
      stride0Depth = 8,
      stride1Depth= 8,
    )
    arch
  }

  def large():Architecture = {
    val arch = Architecture(
      dataType = "SInt_8",
      arraySize = 64,
      dram0Depth = 1024 * 1024 * 2, // 20 bits dram address
      dram1Depth = 1024 * 1024 * 2,
      localDepth = 8192, // 13 bits
      accumulatorDepth = 8192,
      simdRegistersDepth = 1,
      stride0Depth = 8,
      stride1Depth = 8,
    )
    arch
  }
}

object ArchitectureTest extends App{
  val layOut = InstructionLayOut(Architecture.large(),gen = true)
}