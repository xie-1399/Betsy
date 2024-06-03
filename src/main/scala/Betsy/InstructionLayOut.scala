package Betsy

/**
 ** Betsy follow the MiT Licence.(c) xxl, All rights reserved **
 ** Update Time : 2024/4/21      SpinalHDL Version: 1.94       **
 ** You should have received a copy of the MIT License along with this library **
 ** The instruction defines (NoOp/MatMul/DataMove/LoadWeights/SIMD/Configure) here !!! **
 */

import spinal.core._
import spinal.lib._

/* ====== set up the instruction layout here ====== */
case class InstructionLayOut(arch: Architecture){

  def roundSizeBits(size:Int) = {
    val remainder = size % 8
    if(remainder == 0) size else size + 8 - remainder
  }

  val tidSizeBits = log2Up(arch.numberOfThreads)
  val opcodeSizeBits = 3
  val flagsSizeBits = 4
  val headerSizeBits = roundSizeBits(tidSizeBits + opcodeSizeBits + flagsSizeBits)

  //arguments here
  val instructionSizeBytes = 4  // Todo with Instruction width format
  val simdOpSizeBits = log2Up(16) /* 16 ops */
  val simdOperandSizeBits = log2Up(arch.simdRegistersDepth + 1)



  /* generate the NPU config log */
  def genConfigLog() = {

  }

}


/* =============== Instruction Bundle ================= */
/* the object with box (only get and set with the Value)*/
class Box[T](value:T){
  private var _value = value
  def set(value:T) = _value = value
  def get() = _value
}
object Box{
  def apply[T](value:T) = new Box(value)
}

/* ============ feed into the decode unit(Instruction format) ============*/
case class Instruction (instWidth:Int) extends Bundle {
  val opcode = Bits(4 bits) /* show which instruction */
  val flags = Bits(4 bits) /* flags show the behaviour */
  val arguments = Bits((instWidth - 8) bits)
}

/* create the Instruction with kinds of signals */
object Instruction{
  def apply(opcode: Bits, flags: Bundle, arguments: Bundle)(implicit layout: InstructionLayOut): Instruction = {
    val inst = Instruction(layout.instructionSizeBytes * 8)
    inst.opcode := opcode
    inst.flags.assignFromBits(flags.asBits)
    inst.arguments.assignFromBits(arguments.asBits)
    inst
  }

  def fromBits(bit: Bits)(implicit layout: InstructionLayOut): Instruction = {
    val width = layout.instructionSizeBytes * 8
    val opcode = bit(width - 1 downto width - 4)
    val flags = bit(width - 5 downto width - 8)
    val arguments = bit(width - 9 downto 0)
    val inst = Instruction(layout.instructionSizeBytes * 8)
    inst.opcode := opcode
    inst.flags.assignFromBits(flags.asBits)
    inst.arguments.assignFromBits(arguments.asBits)
    inst
  }
}

/* ============ DataMove Instruction ============*/



/* ============ SIMD Instruction ============*/

//simd arguments
case class SIMDArgs(layout:InstructionLayOut) extends Bundle{

  val subInst = ALUInstruction(layout)
  val accReadAddress = UInt()
}
//simd Flags
case class SIMDFlags() extends Bundle{
  val _unused = Bits(1 bits)
  val accumulate = Bool()
  val write = Bool()
  val read = Bool()
}



/* the simd unit driver instruction using the ALU opcode */
case class ALUInstruction(layout: InstructionLayOut) extends Bundle with IMasterSlave {
  val op = UInt(layout.simdOpSizeBits bits)
  val sourceLeft = UInt(layout.simdOperandSizeBits bits) // 0 = io.input, 1 = register 0, ...
  val sourceRight = UInt(layout.simdOperandSizeBits bits)
  val dest = UInt(layout.simdOperandSizeBits bits)

  override def asMaster(): Unit = {
    out(op,dest,sourceLeft,sourceRight)
  }
}

object ALUInstruction{
  def apply(op: BigInt, sourceLeft: BigInt, sourceRight: BigInt, dest: BigInt)(implicit layOut: InstructionLayOut): ALUInstruction = {
    val aluInstruction = new ALUInstruction(layOut)
    aluInstruction.op := op
    aluInstruction.sourceLeft := sourceLeft
    aluInstruction.sourceRight := sourceRight
    aluInstruction.dest := dest
    aluInstruction
  }

  def noOp()(implicit layOut: InstructionLayOut): ALUInstruction = {
    ALUInstruction(ALUOp.NoOp, 0, 0, 0)
  }
}
