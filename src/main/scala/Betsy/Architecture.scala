package Betsy


/**
 ** Betsy follow the MiT Licence.(c) xxl, All rights reserved **
 ** Update Time : 2024/4/21      SpinalHDL Version: 1.94       **
 ** You should have received a copy of the MIT License along with this library **
 ** the config and arch about the Betsy (the arch decides the Instruction Layout) **
 */

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axi.Axi4Config

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
                         numberOfThreads:Int = 1,
                         withSampler:Boolean = true,
                         bandWidth:Int = 256   /* the external bus band width */
                       ) extends ArchitectureDataType(dataType) {
  val dataWidth = dataType.split("_")(1).toInt
  require(dataWidth > 0, "the data width must > 0 !!!")
  val dataKind = dataType.split("_")(0)
  require(dataKind == "SInt" || dataKind == "UInt", "Only Support (SInt,UInt) datatype, Please check the name!!!")
  require(isPow2(arraySize),"the array size should be pow of 2!!!")
  require(arraySize * dataWidth % bandWidth == 0, "external band width error !!!")
  override def sizeBytes: Double = dataWidth / 8
}

/* add more architecture examples here */

object Architecture{

  def embeddings():Architecture = {
    val arch = Architecture(
      dataType = "SInt_4",
      arraySize = 4,
      dram0Depth = 1024 * 1024, // 20 bits dram address
      dram1Depth = 1024 * 1024,
      localDepth = 1024, // 13 bits
      accumulatorDepth = 1024,
      stride0Depth = 8,
      stride1Depth = 8,
      bandWidth = 16
    )
    arch
  }

  def tiny(): Architecture = {
    // 4 + 4 + 16 + 20 + 20 = 64
    val arch = Architecture(
      dataType = "SInt_8",
      arraySize = 8,
      dram0Depth = 1024 * 1024,  // 20 bits dram address
      dram1Depth = 1024 * 1024,
      localDepth = 2048, // max 13 bits
      accumulatorDepth = 2048,
      stride0Depth = 8,
      stride1Depth= 8,
      bandWidth = 64
    )
    arch
  }

  // default config (overflow check)
  def normal():Architecture = {
    val arch = Architecture(
      dataType = "SInt_16",
      arraySize = 64,
      dram0Depth = 1024 * 1024,
      dram1Depth = 1024 * 1024,
      localDepth = 4096,
      accumulatorDepth = 4096,
      stride0Depth = 8,
      stride1Depth = 8,
      bandWidth = 512   // 64 * 16 / 512 = 2
    )
    arch
  }

  def large():Architecture = {
    val arch = Architecture(
      dataType = "SInt_16",
      arraySize = 128,
      dram0Depth = 1024 * 1024 * 2, // 21 bits dram address
      dram1Depth = 1024 * 1024 * 2,
      localDepth = 8192, // 13 bits
      accumulatorDepth = 8192,
      stride0Depth = 8,
      stride1Depth = 8,
      bandWidth = 1024
    )
    arch
  }

  def getWeightBusConfig(arch:Architecture) = Axi4Config(addressWidth = log2Up(arch.dram0Depth) + log2Up(arch.arraySize * arch.dataWidth / 8),
    dataWidth = arch.bandWidth,
    idWidth = -1, useId = false, /* no need for the id*/
    useRegion = false, useBurst = true, useLock = false,
    useCache = false, useSize = true, useQos = false, useLen = true,
    useLast = true, useProt = false)

  def getActivationBusConfig(arch: Architecture) = Axi4Config(addressWidth = log2Up(arch.dram1Depth) + log2Up(arch.arraySize * arch.dataWidth / 8),
    dataWidth = arch.bandWidth,
    idWidth = -1, useId = false, /* no need for the id*/
    useRegion = false, useBurst = true, useLock = false,
    useCache = false, useSize = true, useQos = false, useLen = true,
    useLast = true, useProt = false)

  def maxTransLen: Int = log2Up(256)  /* axi4 bus max trans length */

}


object ArchitectureTest extends App{
  val layOut = InstructionLayOut(Architecture.tiny(),gen = true)
}