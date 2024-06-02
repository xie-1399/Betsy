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
  def sizeBytes:Int
  override def toString: String = name
}

class UInt4(name:String = "UInt4") extends ArchitectureDataType(name){
  override def sizeBytes: Int = 4
}

class SInt4(name:String = "SInt4") extends ArchitectureDataType(name){
  override def sizeBytes: Int = 4
}


case class Architecture( dataType:ArchitectureDataType = new SInt4(),
                         arraySize:Int = 16,
                         dram0Depth: Long = 1024 * 1024,
                         dram1Depth: Long = 1024 * 1024,
                         localDepth: Long = 2048,  /* control the local router move data size */
                         simdRegistersDepth:Int = 1,
                         pcWidth:Int = 32   /* the program counter width */

                       ) {}


case class InstructionLayOut(architecture: Architecture){

  val instructionSizeBytes = 4  // Todo with Instruction width format
  val simdOpSizeBits = log2Up(16) /* 16 ops */
  val simdOperandSizeBits = log2Up(architecture.simdRegistersDepth + 1)
}