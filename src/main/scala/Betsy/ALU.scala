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
/* support with the sign signals with the alu operation */

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

  def isTrue(signal: T): Bool = signal =/= constConvert(signal,0)
  val Zero = constConvert(gen(),0)
  val One = constConvert(gen(),1)
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
    is(ALUOp.Increment){result := increase(sourceLeft)}
    is(ALUOp.Decrement){result := decrease(sourceLeft)}
    is(ALUOp.Add){result := add(sourceLeft,sourceRight)}
    is(ALUOp.Sub){result := sub(sourceLeft,sourceRight)}
    is(ALUOp.Mul){result := mul(sourceLeft,sourceRight)}
    is(ALUOp.Abs){result := abs(sourceLeft)}
    is(ALUOp.GreaterThan){result := Mux(gt(sourceLeft,sourceRight),constConvert(gen(),1),constConvert(gen(),0))}
    is(ALUOp.GreaterThanEqual){result := Mux(gte(sourceLeft,sourceRight),constConvert(gen(),1),constConvert(gen(),0))}
    is(ALUOp.Min){result := min(sourceLeft,sourceRight)}
    is(ALUOp.Max){result := max(sourceLeft,sourceRight)}
  }
}