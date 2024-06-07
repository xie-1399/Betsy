package Betsy

/**
 ** Betsy follow the MiT Licence.(c) xxl, All rights reserved **
 ** Update Time : 2024/4/21      SpinalHDL Version: 1.94       **
 ** You should have received a copy of the MIT License along with this library **
 ** The instruction defines (NoOp/MatMul/DataMove/LoadWeights/SIMD/Configure) here !!! **
 */

/**  Instruction LayOut list like :
 **  [Opcode(4 bits)  Flags(4 bits)  Arguments(XX bits) ]
 **
 **/

import spinal.core._
import spinal.lib._

/* ====== set up the instruction layout here ====== */
case class InstructionLayOut(arch: Architecture){
  /* over with 8 */
  def roundSizeBits(size: Int): Int = {
    val remainder = size % 8
    if (remainder == 0) size else size + 8 - remainder
  }
  val tidSizeBits = log2Up(arch.numberOfThreads)
  val opcodeSizeBits = 3
  val flagsSizeBits = 4
  val headerSizeBits = roundSizeBits(tidSizeBits + opcodeSizeBits + flagsSizeBits) /* is about 8 size if numThreads <= 2 */

  /* about address */
  val localOperandSizeBits = log2Up(arch.localDepth)
  val dram0OperandSizeBits = log2Up(arch.dram0Depth)
  val dram1OperandSizeBits = log2Up(arch.dram1Depth)
  val accumulatorOperandSizeBits = log2Up(arch.accumulatorDepth)
  val stride0SizeBits = log2Up(arch.stride0Depth) /* like 2 or 8 */
  val stride1SizeBits = log2Up(arch.stride1Depth) /* like 2 or 8 */

  /* simd alu instruction */
  val simdOpSizeBits = log2Up(ALUOp.allAlus) /* 16 sub-simd ops -> 4 bits*/
  val simdOperandSizeBits = log2Up(arch.simdRegistersDepth + 1) /* simd register number */
  val simdInstructionSizeBits = simdOpSizeBits + 3 * simdOperandSizeBits /* simd sub-instruction width */
  val operand0AddressSizeBits = List(
    localOperandSizeBits, // MatMul, DataMove, LoadWeights
    accumulatorOperandSizeBits // SIMD
  ).max
  val operand0SizeBits = roundSizeBits(
    operand0AddressSizeBits + stride0SizeBits
  )
  val operand0Padding =
    operand0SizeBits - (operand0AddressSizeBits + stride0SizeBits)
  val operand1AddressSizeBits = List(
    localOperandSizeBits, // LoadWeights
    dram0OperandSizeBits, // DataMove
    dram1OperandSizeBits, // DataMove
    accumulatorOperandSizeBits // MatMul, DataMove, SIMD
  ).max
  val operand1SizeBits =
    roundSizeBits(
      operand1AddressSizeBits + stride1SizeBits
    )
  val operand1Padding =
    operand1SizeBits - (operand1AddressSizeBits + stride1SizeBits)
  val operand2AddressSizeBits = List(
    List(
      localOperandSizeBits,
      accumulatorOperandSizeBits
    ).min, // MatMul, DataMove
    List(localOperandSizeBits, dram0OperandSizeBits).min, // DataMove
    List(localOperandSizeBits, dram1OperandSizeBits).min, // DataMove
    simdInstructionSizeBits // SIMD
  ).max
  val operand2SizeBits =
    roundSizeBits(
      operand2AddressSizeBits
    )
  val operand2Padding = operand2SizeBits - operand2AddressSizeBits
  val operandsSizeBits = operand0SizeBits + operand1SizeBits + operand2SizeBits
  val instructionSizeBytes =
    (headerSizeBits + operandsSizeBits) / 8

  override def toString: String = {
    "Betsy Instruction Layout"
  }

  /* generate the NPU config log */
  def genConfigLog() = {

  }
}

object InstructionLayOut{

  def apply(arch: Architecture): InstructionLayOut = {
    val layOut = new InstructionLayOut(arch)
    layOut.genConfigLog()  /* generate the log files */
    layOut
  }

}


/* =============== Instruction Bundle ================= */
/* ============ feed into the decode unit (Instruction format) ============*/
case class InstructionFormat (instWidth:Int) extends Bundle {
  val opcode = Bits(4 bits) /* show which instruction */
  val flags = Bits(4 bits) /* flags show the behaviour */
  val arguments = Bits((instWidth - 8) bits)
}

/* create the Instruction with kinds of signals */
object InstructionFormat{
  def apply(opcode: Bits, flags: Bundle, arguments: Bundle)(implicit layout: InstructionLayOut): InstructionFormat = {
    val inst = InstructionFormat(layout.instructionSizeBytes * 8)
    inst.opcode := opcode
    inst.flags.assignFromBits(flags.asBits)
    inst.arguments.assignFromBits(arguments.asBits)
    inst
  }

  def fromBits(bit: Bits)(implicit layout: InstructionLayOut): InstructionFormat = {
    val width = layout.instructionSizeBytes * 8
    val opcode = bit(width - 1 downto width - 4)
    val flags = bit(width - 5 downto width - 8)
    val arguments = bit(width - 9 downto 0)
    val inst = InstructionFormat(layout.instructionSizeBytes * 8)
    inst.opcode := opcode
    inst.flags.assignFromBits(flags.asBits)
    inst.arguments.assignFromBits(arguments.asBits)
    inst
  }
}


/* the simd unit driver instruction using the ALU opcode */
case class SIMDInstruction(layout: InstructionLayOut) extends Bundle with IMasterSlave {
  val op = UInt(layout.simdOpSizeBits bits)
  val sourceLeft = UInt(layout.simdOperandSizeBits bits) // 0 = io.input, 1 = register 0, ...
  val sourceRight = UInt(layout.simdOperandSizeBits bits)
  val dest = UInt(layout.simdOperandSizeBits bits)

  override def asMaster(): Unit = {
    out(op,dest,sourceLeft,sourceRight)
  }
}

object SIMDInstruction{
  def apply(op: BigInt, sourceLeft: BigInt, sourceRight: BigInt, dest: BigInt)(implicit layOut: InstructionLayOut): SIMDInstruction = {
    val aluInstruction = new SIMDInstruction(layOut)
    aluInstruction.op := op
    aluInstruction.sourceLeft := sourceLeft
    aluInstruction.sourceRight := sourceRight
    aluInstruction.dest := dest
    aluInstruction
  }

  def noOp()(implicit layOut: InstructionLayOut): SIMDInstruction = {
    SIMDInstruction(ALUOp.NoOp, 0, 0, 0)
  }
}