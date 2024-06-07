package Betsy

/**
 ** Betsy follow the MiT Licence.(c) xxl, All rights reserved **
 ** Update Time : 2024/4/14      SpinalHDL Version: 1.94       **
 ** You should have received a copy of the MIT License along with this library **
 **  **
 */

import spinal.core._
import spinal.lib._

/* the real instruction opcode */
object Opcode {
  def NoOp: Bits = B(0x0, 4 bits)
  def MatMul: Bits = B(0x1, 4 bits)
  def DataMove: Bits = B(0x2, 4 bits)
  def LoadWeights: Bits = B(0x3, 4 bits)
  def SIMD: Bits = B(0x4, 4 bits)
  def Configure: Bits = B(0xF, 4 bits)
  def all: Array[Bits] = Array(NoOp, MatMul, DataMove, LoadWeights, SIMD, Configure)
  def Operror(op: Bits): Bool = {
    !(op === NoOp || op === MatMul || op === DataMove || op === LoadWeights || op === SIMD || op === Configure)
  }
}


/* the sub simd instruction opcode */
object ALUOp{
  val numOps = 16

  // opcodes
  val NoOp = 0
  val Zero = 1
  val Move = 2

  // binary
  val Not = 3
  val And = 4
  val Or = 5

  // arithmetic
  val Increment = 6
  val Decrement = 7
  val Add = 8
  val Sub = 9
  val Mul = 10
  val Abs = 11

  // comparative
  val GreaterThan = 12
  val GreaterThanEqual = 13
  val Min = 14
  val Max = 15

  // one op plays role
  def isUnary(op:UInt):Bool = {
    val unaryOps = Array(Move, Not, Increment, Decrement, Abs)
    unaryOps.map(op === _).reduce(_ && _)
  }
  def allAlus = 16

}


