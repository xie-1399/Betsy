package Betsy

/**
 ** Betsy follow the MiT Licence.(c) xxl, All rights reserved **
 ** Update Time : 2024/4/21      SpinalHDL Version: 1.94       **
 ** You should have received a copy of the MIT License along with this library **
 ** **
 */

import spinal.core._
import spinal.lib._

class Instruction {

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
