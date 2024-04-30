package Betsy

/**
 ** Betsy follow the MiT Licence.(c) xxl, All rights reserved **
 ** Update Time : 2024/4/21      SpinalHDL Version: 1.94       **
 ** You should have received a copy of the MIT License along with this library **
 ** The instruction defines (NoOp/MatMul/DataMove/LoadWeights/SIMD/Configure) here !!! **
 */

import spinal.core._
import spinal.lib._

/* the object with box (only get and set with the Value)*/
class Box[T](value:T){
  private var _value = value
  def set(value:T) = _value = value
  def get() = _value
}

object Box{
  def apply[T](value:T) = new Box(value)
}

/* feed into the decode unit*/
case class Instruction (instWidth:Int) extends Bundle {
  val opcode = Bits(4 bits) /* show which instruction */
  val flags = Bits(4 bits) /* flags show the behaviour */
  val arguments = Bits((instWidth - 8) bits)
}

/* create the Instruction with kinds of signals */
object Instruction{
  def apply(opcode:Bits,flags:Bundle,arguments:Bundle)(implicit layout:InstructionLayOut) = {
    // val inst = Instruction(layout.instructionSizeBytes * 8)
    // inst.opcode
  }

  def apply(opcode:Bits,arguments:Bundle) = {

  }

  def apply(opcode:Bits) = {

  }

  def fromBits() = {

  }
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
