package Betsy.Instruction

import spinal.core._
import spinal.lib._
import Betsy._

/**
 ** Betsy follow the MiT Licence.(c) xxl, All rights reserved **
 ** Update Time : 2024/6/5      SpinalHDL Version: 1.94       **
 ** You should have received a copy of the MIT License along with this library **
 */


/**
 ** Load weight from memory address into systolic array
 ** Opcode = 0x3
 ** Flags show whether is load zero?
 ** Argument list :
 ** OP0 Local Memory stride/address（with padding unused to arrive time * 8）
 ** OP1 is load size
 ** No Op2
 ** for example :  the instruction width is 4 + 4 + 16(just padding) + 24(size likely) + 16(13 + 3)
 ** 0011 0000 0000000000000000 000000000000000000010000 1000000010000000 (from the address 126 load 16 data with stride 4 into the )
 **/


case class LoadWeightArgs(layOut: InstructionLayOut) extends Bundle {
  val size = UInt(layOut.operand1SizeBits bits)
  val _unused = Bits(layOut.operand0Padding bits)
  val stride = UInt(layOut.stride0SizeBits bits)
  val address = UInt(layOut.operand0AddressSizeBits bits)
}

object LoadWeightArgs{
  def apply(address: UInt, stride: UInt, size: UInt)(implicit layOut: InstructionLayOut): LoadWeightArgs = {
    val arguments = LoadWeightArgs(layOut)
    arguments.address := address.resized
    arguments.stride := stride.resized
    arguments.size := size.resized
    arguments._unused.clearAll()
    arguments
  }

  def fromBits(op0: Bits, op1: Bits)(implicit layOut: InstructionLayOut): LoadWeightArgs = {
    val address = op0(layOut.operand0AddressSizeBits - 1 downto 0).asUInt
    val stride = op0(layOut.stride0SizeBits + layOut.operand0AddressSizeBits - 1  downto layOut.operand0AddressSizeBits).asUInt
    val size = op1.asUInt
    apply(address, stride, size)
  }
}

case class LoadWeightFlags() extends Bundle{
  val _unused = UInt(3 bits)
  val zeroes = Bool()
}

object LoadWeightFlags{
  def apply(zeroes: Bool): LoadWeightFlags = {
    val flags = LoadWeightFlags()
    flags.zeroes := zeroes
    flags._unused.clearAll()
    flags
  }

  def isValid(flags: Bits): Bool = {
    flags.asUInt <= 1
  }
}