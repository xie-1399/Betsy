package Betsy.Instruction

/**
 ** Betsy follow the MiT Licence.(c) xxl, All rights reserved **
 ** Update Time : 2024/6/6      SpinalHDL Version: 1.94       **
 ** You should have received a copy of the MIT License along with this library **
 */


/**
 ** Perform computations on data in the accumulator
 ** Opcode = 0x4
 ** Flags contains Read? Write? Accumulate?
 ** Argument list :
 ** OP0 Accumulator write address
 ** OP1 Accumulator read address
 ** Op2 SIMD sub-instruction
 ** for example :  Todo add instruction example
 **/


import spinal.core._
import spinal.lib._
import Betsy._

case class SIMDArgs(layOut: InstructionLayOut) extends Bundle {
  private val paddingWidth =
    layOut.operand2SizeBits - layOut.simdInstructionSizeBits
  val _unused = Bits(paddingWidth bits)
  val instruction = SIMDInstruction(layOut)
  val accReadAddress = UInt(layOut.operand1SizeBits bits)
  val accWriteAddress = UInt(layOut.operand0SizeBits bits)
}


object SIMDArgs {
  def apply(accReadAddress: UInt,
            accWriteAddress: UInt,
            instruction: SIMDInstruction)(implicit layOut: InstructionLayOut): SIMDArgs = {
    val arguments = SIMDArgs(layOut)
    arguments._unused.clearAll()
    arguments.instruction.op := instruction.op.resized
    arguments.instruction.sourceLeft := instruction.sourceLeft.resized
    arguments.instruction.sourceRight := instruction.sourceRight.resized
    arguments.instruction.dest := instruction.dest.resized
    arguments.accReadAddress := accReadAddress.resized
    arguments.accWriteAddress := accWriteAddress.resized
    arguments
  }

  // Todo check the instruction format
  def fromBits(op0:Bits, op1:Bits, op2:Bits)(implicit layOut:InstructionLayOut) = {
    val accWriteAddress = op0.asUInt
    val accReadAddress = op1.asUInt
    val simdInstruction = SIMDInstruction(layOut)
    simdInstruction.dest := op2(layOut.simdOperandSizeBits - 1 downto 0).asUInt.resized
    simdInstruction.sourceRight := op2(layOut.simdOperandSizeBits * 2 - 1 downto layOut.simdOperandSizeBits).asUInt.resized
    simdInstruction.sourceLeft := op2(layOut.simdOperandSizeBits * 3 - 1 downto layOut.simdOperandSizeBits * 2).asUInt.resized
    simdInstruction.op := op2(layOut.simdOperandSizeBits * 3 + layOut.simdOpSizeBits downto layOut.simdOperandSizeBits * 3).asUInt.resized
    apply(accReadAddress,accWriteAddress,simdInstruction)
  }
}


case class SIMDFlags() extends Bundle {
  val _unused = Bits(1 bits)
  val accumulate = Bool() // whether to accumulate (true) or overwrite when writing
  val write = Bool() // whether to write to the given accumulator address
  val read  = Bool() // whether to read from the given accumulator address
}

object SIMDFlags{
  def apply(accumulate: Bool, write: Bool, read: Bool): SIMDFlags = {
    val flags = SIMDFlags()
    flags._unused.clearAll()
    flags.accumulate := accumulate
    flags.read := read
    flags.write := write
    flags
  }
  def isValid(flags: UInt): Bool = flags < 8
}