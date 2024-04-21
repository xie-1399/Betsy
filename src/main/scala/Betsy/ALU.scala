package Betsy

/**
 ** Betsy follow the MiT Licence.(c) xxl, All rights reserved **
 ** Update Time : 2024/4/21      SpinalHDL Version: 1.94       **
 ** You should have received a copy of the MIT License along with this library **
 ** Todo be Tested !!!**
 */

import spinal.core._
import spinal.lib._
import Until._

class ALU[T <: Data with Num[T]](gen:HardType[T],numOps:Int, numRegisters:Int,
                                 inputPipe:Boolean = false,outputPipe:Boolean = false) extends BetsyModule {
  val io = new Bundle{
    val op = in(UInt(log2Up(numOps) bits))
    val input = in(gen())
    val sourceLeft = in(UInt(log2Up(numRegisters + 1) bits)) /* 0 is input and 1 is registers 0 ...*/
    val sourceRight = in(UInt(log2Up(numRegisters + 1) bits)) /* 0 is input and 1 is registers 0 ...*/
    val dest = in(UInt(log2Up(numRegisters + 1) bits)) /* 0 is input and 1 is registers 0 ...*/
    val output = out(gen())
  }

  def isTrue(signal: T): Bool = !(signal === zero(signal))
  val Zero = zero(gen())
  val One = one(gen())
  val registers = Vec(Reg(gen).init(Zero),numRegisters)

  val op = if(inputPipe) RegNext(io.op).init(0) else io.op
  val input = if(inputPipe) RegNext(io.input).init(Zero) else io.input
  val sourceLeftInput = if(inputPipe) RegNext(io.sourceLeft).init(0) else io.sourceLeft
  val sourceRightInput = if(inputPipe) RegNext(io.sourceRight).init(0) else io.sourceRight
  val destInput = if(inputPipe) RegNext(io.dest).init(0) else io.dest

  val sourceLeft = Mux(sourceLeftInput === 0,input,registers((sourceLeftInput - U(1)).resized))
  val sourceRight = Mux(sourceRightInput === 0,input,registers((sourceLeftInput - U(1)).resized))

  val result = gen()
  result := input /* default op is NoOp */
  val output = if (outputPipe) RegNext(result).init(Zero) else result
  io.output := output
  val dest = Demux(destInput === 0 || op === ALUOp.NoOp,io.output,registers((sourceLeftInput - U(1)).resized))
  dest := result /* write the result into the register or io.out */

  /* alu operations(overflow is cut down ) Todo with the SFix */
  switch(op){
    is(ALUOp.Zero){result := Zero}
    is(ALUOp.Move){result := sourceLeft} /* left -> out */
    is(ALUOp.Not){ result := Mux(isTrue(sourceLeft),Zero,One) } /* source left is zero -> One not zero -> zero */
    is(ALUOp.And){ result := Mux(isTrue(sourceLeft) && isTrue(sourceRight),One,Zero) }
    is(ALUOp.Or){ result := Mux(isTrue(sourceLeft) || isTrue(sourceRight),One,Zero) }
    is(ALUOp.Increment){result := upDown(sourceLeft + One,gen.craft())}
    is(ALUOp.Decrement){result := upDown(sourceLeft - One,gen.craft())}
    is(ALUOp.Add){result := upDown(sourceLeft + sourceRight,gen.craft())}
    is(ALUOp.Sub){result := upDown(sourceLeft - sourceRight,gen.craft())}
    is(ALUOp.Mul){result := upDown(sourceLeft * sourceRight,gen.craft()).resized}
    is(ALUOp.Abs){
      val signal = gen().isInstanceOf[SInt]
      val abs = if(signal) sourceLeft.asInstanceOf[SInt].abs else sourceLeft
      result.assignFromBits(abs.asBits)
    }
    is(ALUOp.GreaterThan){result := Mux(sourceLeft > sourceRight,One,Zero)}
    is(ALUOp.GreaterThanEqual){result := Mux(sourceLeft >= sourceRight,One,Zero)}
    is(ALUOp.Min){result := Mux(sourceLeft > sourceRight,sourceRight,sourceLeft)}
    is(ALUOp.Max){result := Mux(sourceLeft > sourceRight,sourceLeft,sourceRight)}
  }
}

object ALU extends App{
  SpinalVerilog(new ALU(SInt(8 bits),numOps = 15,numRegisters = 1)) /* no insert registers */
}