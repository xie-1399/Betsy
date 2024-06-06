package Betsy.Instruction

/**
 ** Betsy follow the MiT Licence.(c) xxl, All rights reserved **
 ** Update Time : 2024/6/5      SpinalHDL Version: 1.94       **
 ** You should have received a copy of the MIT License along with this library **
 */
import Betsy._
import spinal.core._
import spinal.lib._

/**
 ** Load input at memory address into systolic array and store result at accumulator address
 ** Opcode = 0x1
 ** Flags contains (accumulate/zero) padding with 2 bits
 ** Argument list :
 ** OP0 Local Memory stride/address（with padding unused to arrive time * 8）
 ** OP1 Accumulator stride/address (with Padding)
 ** OP2 Accumulator Size
 ** for example :  Todo add instruction example
 **/

case class MatMulArgs(layOut: InstructionLayOut) extends Bundle {
  val size = UInt(layOut.operand2SizeBits bits)
  val _unused1 = Bits(layOut.operand1Padding bits)
  val accStride = UInt(layOut.stride1SizeBits bits)
  val accAddress = UInt(layOut.operand1AddressSizeBits bits)
  val _unused0 = Bits(layOut.operand0Padding bits)
  val memStride = UInt(layOut.stride0SizeBits bits)
  val memAddress = UInt(layOut.operand0AddressSizeBits bits)
}

object MatMulArgs{
  def apply(memAddress: UInt,
            accAddress: UInt,
            size: UInt,
            memStride: UInt,
            accStride: UInt)(implicit layOut: InstructionLayOut): MatMulArgs = {
    val argument = MatMulArgs(layOut)
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


case class MatMulFlags() extends Bundle{
  val _unused = UInt(2 bits)
  val zeroes = Bool()
  val accumulate = Bool()
}

object MatMulFlags{
  /* flag contains two Unused bits */
  def apply(zeroes: Bool, accumulate: Bool): MatMulFlags = {
    val flag = MatMulFlags()
    flag.zeroes := zeroes
    flag.accumulate := accumulate
    flag
  }
  def isValid(flags:UInt):Bool = flags < 4
}