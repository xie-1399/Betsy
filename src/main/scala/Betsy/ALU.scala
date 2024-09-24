package Betsy

/**
 ** Betsy follow the MiT Licence.(c) xxl, All rights reserved **
 ** Update Time : 2024/4/21      SpinalHDL Version: 1.94       **
 ** You should have received a copy of the MIT License along with this library **
 ** the alu can operation between the input / registers / output values **
 ** Test Status : PASS :)         Version:0.1**
 */

import spinal.core._
import Until._
import Operations._
/* support with the sign signals with the alu operation

* Todo with the SFix Datatype */

class ALU[T <: Data](gen:HardType[T],numOps:Int, numRegisters:Int,
                                 inputPipe:Boolean = false,outputPipe:Boolean = false) extends BetsyModule {
  val io = new Bundle{
    val op = in(UInt(log2Up(numOps) bits))
    val input = in(gen())
    val sourceLeft = in(UInt(log2Up(numRegisters + 1) bits)) /* 0 is input and 1 is registers 0 ...*/
    val sourceRight = in(UInt(log2Up(numRegisters + 1) bits)) /* 0 is input and 1 is registers 0 ...*/
    val dest = in(UInt(log2Up(numRegisters + 1) bits)) /* 0 is input and 1 is registers 0 ...*/
    val output = out(gen())
  }

  def isTrue(signal: T): Bool = signal =/= zero(signal)
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

  val output = if (outputPipe) RegNext(result).init(Zero) else result
  io.output := output
  when(destInput =/= 0 && op =/= ALUOp.NoOp){
    registers((sourceLeftInput - U(1)).resized) := result
  }

  /* alu operations(some clip is about the Up Down and abs) */
  result := input /* default op is NoOp */
  switch(op){
    is(ALUOp.Zero){result := Zero}
    is(ALUOp.Move){result := sourceLeft} /* move the register value out */
    is(ALUOp.Not){ result := Mux(isTrue(sourceLeft),Zero,One) } /* source left is zero -> One not zero -> zero */
    is(ALUOp.And){ result := Mux(isTrue(sourceLeft) && isTrue(sourceRight),One,Zero) }
    is(ALUOp.Or){ result := Mux(isTrue(sourceLeft) || isTrue(sourceRight),One,Zero) }
//    is(ALUOp.Increment){result := upDown(sourceLeft +^ One,gen.craft()).resized}
//    is(ALUOp.Decrement){result := upDown(sourceLeft -^ One,gen.craft()).resized}
    is(ALUOp.Add){
      result := resizePoint(upDown(add(gen.craft(),sourceLeft,sourceRight),gen.craft()),gen.craft())
    }
//    is(ALUOp.Sub){result := upDown(sourceLeft -^ sourceRight,gen.craft()).resized}
//    is(ALUOp.Mul){result := upDown(sourceLeft * sourceRight,gen.craft()).resized}
//    is(ALUOp.Abs){
//      val value = sourceLeft.asInstanceOf[SInt]
//      when(value === S(min(gen()))){  /* let the absolute -128 -> 127 */
//        result.assignFromBits(S(value.maxValue, result.getBitsWidth bits).asBits)
//      }.otherwise{
//        val abs = sourceLeft.asInstanceOf[SInt].abs
//        result.assignFromBits(abs.asBits)}
//      }
//    is(ALUOp.GreaterThan){result := Mux(sourceLeft > sourceRight,One,Zero)}
//    is(ALUOp.GreaterThanEqual){result := Mux(sourceLeft >= sourceRight,One,Zero)}
//    is(ALUOp.Min){result := Mux(sourceLeft > sourceRight,sourceRight,sourceLeft)}
//    is(ALUOp.Max){result := Mux(sourceLeft > sourceRight,sourceLeft,sourceRight)}
  }
}

object ALU extends App{
  SpinalSystemVerilog(new ALU(SFix(16 exp, -2 exp),16,2))
}