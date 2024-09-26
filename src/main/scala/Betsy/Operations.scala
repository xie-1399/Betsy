package Betsy

/**
 ** Betsy follow the MiT Licence.(c) xxl, All rights reserved **
 ** Update Time : 2024/9/24      SpinalHDL Version: 1.94       **
 ** You should have received a copy of the MIT License along with this library **
 ** the alu and mac operations in the accelerator**
 */

import spinal.core._
import spinal.lib._
import Betsy.Until._

// all the operations will get the right value(with carry)
// the overflow will control the output in the range


object Operations {

  //Todo with AFix check and test

  // Add with carry
  def add[T <: Data with Num[T]](m1:T, m2:T, overflow:Boolean = true): T = {
    val res = if(overflow) m1 +| m2 else m1 +^ m2
    res
  }
  // Sub
  def sub[T <: Data with Num[T]](m1: T, m2: T, overflow:Boolean = true): T = {
    val res = if(overflow) m1 -| m2 else m1 -^ m2
    res
  }
  // Mul
  def mul[T <: Data with Num[T]](m1: T, m2: T, overflow:Boolean = true): T = {
    val cut_width = m1.getBitsWidth
    val res = if(overflow){
      (m1 * m2).sat(cut_width)
    }
    else m1 * m2
    res
  }
  // Divide (no overflow control)
  def divide[T <: Data with Num[T]](m1: T, m2: T): T = {
    val res = m1 / m2
    res
  }
  // Increase
  def increase[T <: Data with Num[T]](source: T): T = {
    val res = source +| constConvert(source,1)
    res
  }
  // Decrease
  def decrease[T <: Data with Num[T]](source: T): T = {
    val res = source -| constConvert(source,1)
    res
  }
  // Abs
  def abs[T <: Data with Num[T]](source: T): T = {
    val res = source match {
      case _:UInt => source
      case _:SInt => {
        val abs = source.asInstanceOf[SInt].absWithSym
        val toSInt = S(0,1 bits) @@ abs.asSInt
        toSInt
      }
      case _:AFix => {
        val fix_abs = if(source.asInstanceOf[AFix].signed){
          //Todo
          //source.asInstanceOf[AFix].asUFix()
        }else{
          source
        }
      }
    }
    res.asInstanceOf[T]
  }
  // Greater than
  def gt[T <: Data with Num[T]](m1: T, m2: T): Bool = {
    val res = m1 > m2
    res
  }
  // Greater than equal
  def gte[T <: Data with Num[T]](m1: T, m2: T): Bool = {
    val res = m1 >= m2
    res
  }
  // Less than
  def lt[T <: Data with Num[T]](m1: T, m2: T): Bool = {
    val res = m1 < m2
    res
  }
  // Less than equal
  def lte[T <: Data with Num[T]](m1: T, m2: T): Bool = {
    val res = m1 <= m2
    res
  }
  // MAC
  def mac[T <: Data with Num[T]](m1: T, m2: T, acc: T): T = {
    val res = add(mul(m1,m2),acc)
    res
  }
  // Min between two values
  def min[T <: Data with Num[T]](m1: T, m2: T): T = {
    val res = Mux(gt(m1,m2), m2, m1)
    res
  }
  // Max
  def max[T <: Data with Num[T]](m1: T, m2: T): T = {
    val res = Mux(gt(m1, m2), m1, m2)
    res
  }
  // Square
  def square[T <: Data with Num[T]](source:T):T = {
    val res = mul(source, source)
    res
  }
  // Cube
  def cube[T <: Data with Num[T]](source: T): T = {
    val res = mul(mul(source,source),source)
    res
  }

}
