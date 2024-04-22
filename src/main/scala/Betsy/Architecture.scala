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
                         dram0Depth: Long = 1048576,
                         dram1Depth: Long = 1048576,
                         simdRegistersDepth:Int = 1
                       ) {}


case class InstructionLayOut(architecture: Architecture){

  val simdOpSizeBits = log2Up(15) /* 16 ops */
  val simdOperandSizeBits = log2Up(architecture.simdRegistersDepth + 1)
}