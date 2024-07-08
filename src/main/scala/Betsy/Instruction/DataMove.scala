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
 ** for example : generate dram0 -> memory instruction (from dram0 A:4 S: 0 to local memory A:8 S:2) with size 16
 ** 0010 0000 000000000001000 0000000000000000000000100 0001000000001000
 **/

import spinal.core._
import spinal.lib._
import Betsy._

case class DataMoveArgs(layOut:InstructionLayOut) extends Bundle {
  val size = UInt(layOut.operand2SizeBits bits)
  val _unused1 = Bits(layOut.operand1Padding bits)
  val accStride = UInt(layOut.stride1SizeBits bits) /* also the dram */
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

  //from the bits to send the argument value
  def fromBits(op0:Bits,op1:Bits,op2:Bits)(implicit layOut: InstructionLayOut): DataMoveArgs = {
    val memAddress = op0(layOut.operand0AddressSizeBits - 1 downto 0).asUInt
    val memStride = op0(layOut.stride0SizeBits + layOut.operand0AddressSizeBits - 1  downto layOut.operand0AddressSizeBits).asUInt
    val accAddress = op1(layOut.operand1SizeBits- 1 downto 0).asUInt
    val accStride = op1(layOut.stride1SizeBits + layOut.operand1AddressSizeBits - 1  downto layOut.operand1AddressSizeBits).asUInt
    val size = op2.asUInt
    apply(memAddress,accAddress,size,memStride,accStride)
  }
}

case class DataMoveFlags() extends Bundle{
  val kind = UInt(4 bits)
}

object DataMoveKind {
  type DataMoveKind = UInt
  def dram0ToMemory: UInt = U(0, 4 bits)
  def memoryToDram0: UInt = U(1, 4 bits)
  def dram1ToMemory: UInt = U(2, 4 bits)
  def memoryToDram1: UInt = U(3, 4 bits)
  def accumulatorToMemory: UInt = U(0xc, 4 bits)
  def memoryToAccumulator: UInt = U(0xd, 4 bits)
  def memoryToAccumulatorAccumulate: UInt = U(0xf, 4 bits)

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