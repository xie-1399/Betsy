package Betsy.Instruction

/**
 ** Betsy follow the MiT Licence.(c) xxl, All rights reserved **
 ** Update Time : 2024/6/5      SpinalHDL Version: 1.94       **
 ** You should have received a copy of the MIT License along with this library **
 */


/**
 ** Move data between the main memory and either the accumulators or one of two off-chip DRAMs
 ** Opcode = 0x2
 ** Flags contains (instruction dataflow)
 ** Argument list :
 ** OP0 Local Memory stride/address（with padding unused to arrive time * 8）
 ** OP1 Accumulator or DRAM stride/address (with Padding)
 ** OP2 Accumulator Size
 ** for example :  Todo add instruction example
 **/

import spinal.core._
import spinal.lib._
import Betsy._

case class DataMoveArgs(layOut:InstructionLayOut) extends Bundle {
  val size = UInt(layOut.operand2SizeBits bits)
  val _unused1 = Bits(layOut.operand1Padding bits)
  val accStride = UInt(layOut.stride1SizeBits bits)
  val accAddress = UInt(layOut.operand1AddressSizeBits bits)
  val _unused0 = Bits(layOut.operand0Padding bits)
  val memStride = UInt(layOut.stride0SizeBits bits)
  val memAddress = UInt(layOut.operand0AddressSizeBits bits)
}

object DataMoveArgs{
  def apply(memAddress: UInt,
            accAddress: UInt,
            size: UInt,
            memStride: UInt,
            accStride: UInt)(implicit layOut: InstructionLayOut): DataMoveArgs = {
    val argument = DataMoveArgs(layOut)
    argument.size := size.resized
    argument._unused1.clearAll()
    argument.accStride := accStride.resized
    argument.accAddress := accAddress.resized
    argument._unused0.clearAll()
    argument.memStride := memStride.resized
    argument.memAddress := memAddress.resized
    argument
  }
}

case class DataMoveFlags() extends Bundle{
  import DataMoveKind.DataMoveKind
  val kind = DataMoveKind(UInt(4 bits))
}

object DataMoveKind {
  type DataMoveKind = UInt

  val dram0ToMemory = U(0,4 bits)
  val memoryToDram0 = U(1,4 bits)
  val dram1ToMemory = U(2,4 bits)
  val memoryToDram1 = U(3,4 bits)

  //update it
  val accumulatorToMemory = U(0xc,4 bits)
  val memoryToAccumulator = U(0xb,4 bits)
  val memoryToAccumulatorAccumulate = U(0xa)

  val all = Array(
    dram0ToMemory,
    memoryToDram0,
    dram1ToMemory,
    memoryToDram1,
    accumulatorToMemory,
    memoryToAccumulator,
    memoryToAccumulatorAccumulate
  )

  def apply(kind: UInt): DataMoveKind = kind.resized

  def isValid(kind: UInt): Bool = {
    kind <= memoryToDram1 ||
      kind === accumulatorToMemory ||
      kind === memoryToAccumulator ||
      kind === memoryToAccumulatorAccumulate
  }
}