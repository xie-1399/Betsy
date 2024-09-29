package Betsy

/**
 ** Betsy follow the MiT Licence.(c) xxl, All rights reserved **
 ** Update Time : 2024/9/24      SpinalHDL Version: 1.94       **
 ** You should have received a copy of the MIT License along with this library **
 ** the alu and mac operations in the accelerator**
 */

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import Betsy.Until._
import scala.math

// all the operations will get the right value(with carry)
// the overflow will control the output in the range


object Operations {

  // Add
  def add[T <: Data with Num[T]](m1:T, m2:T, overflow:Boolean = true): T = {
    val res = (m1, m2) match {
      case (m1: AFix, m2: AFix) => {
        if(overflow){
          (m1 + m2).sat(m1).truncated
        }
        else m1 + m2
      }
      case (m1: SInt, m2: SInt) => {
        if(overflow) m1 +| m2 else m1 +^ m2
      }
      case (m1: UInt, m2: UInt) => {
        if (overflow) m1 +| m2 else m1 +^ m2
      }
      case _ => {
        assert(true, s"Only three data types, SInt, UInt and AFix are supported")
      }
    }
    res.asInstanceOf[T]
  }
  // Sub
  def sub[T <: Data with Num[T]](m1: T, m2: T, overflow:Boolean = true): T = {
    val res = (m1, m2) match {
      case (m1: AFix, m2: AFix) => {
        if(overflow){
          (m1 - m2).sat(m1).truncated
        }
        else m1 - m2
      }
      case (m1: SInt, m2: SInt) => {
        if(overflow) m1 -| m2 else m1 -^ m2
      }
      case (m1: UInt, m2: UInt) => {
        if (overflow) m1 -| m2 else m1 -^ m2
      }
      case _ => {
        assert(true, s"Only three data types, SInt, UInt and AFix are supported")
      }
    }
    res.asInstanceOf[T]
  }
  // Mul
  def mul[T <: Data with Num[T]](m1: T, m2: T, overflow:Boolean = true): T = {
    val cut_width = m1.getBitsWidth
    val res = (m1, m2) match {
      case (m1: AFix, m2: AFix) => {
        if(overflow){
          (m1 * m2).sat(m1).truncated
        }
        else m1 * m2
      }
      case (m1: SInt, m2: SInt) => {
        if(overflow){
          (m1 * m2).sat(cut_width)
        }
        else m1 * m2
      }
      case (m1: UInt, m2: UInt) => {
        if(overflow){
          (m1 * m2).sat(cut_width)
        }
        else m1 * m2
      }
      case _ => {
        assert(true, s"Only three data types, SInt, UInt and AFix are supported")
      }
    }
    res.asInstanceOf[T]
  }

  // Divide (no overflow control) Todo with the fixed divede
//  def divide[T <: Data with Num[T]](m1: T, m2: T): T = {
//    val res = (m1, m2) match {
//      case (m1: AFix, m2: AFix) => (m1 / m2).truncated
//      case _ =>
//    }
//    res.asInstanceOf[T]
//  }

  // Increase
  def increase[T <: Data with Num[T]](source: T, overflow:Boolean = true): T = {
    val res = source match {
      case source: AFix => {
        if (overflow) {
          (source + constConvert(source, 1)).sat(source).truncated
        }
        else source +^ constConvert(source, 1)
      }
      case source: SInt => {
        if (overflow) source +| constConvert(source, 1) else source +^ constConvert(source, 1)
      }
      case source: UInt => {
        if (overflow) source +| constConvert(source, 1) else source +^ constConvert(source, 1)
      }
      case _ => {
        assert(true, s"Only three data types, SInt, UInt and AFix are supported")
      }
    }
    res.asInstanceOf[T]
  }
  // Decrease
  def decrease[T <: Data with Num[T]](source: T, overflow:Boolean = true): T = {
    val res = source match {
      case source: AFix => {
        if (overflow) {
          (source - constConvert(source, 1)).sat(source).truncated
        }
        else source -^ constConvert(source, 1)
      }
      case source: SInt => {
        if (overflow) source -| constConvert(source, 1) else source -^ constConvert(source, 1)
      }
      case source: UInt => {
        if (overflow) source -| constConvert(source, 1) else source -^ constConvert(source, 1)
      }
      case _ => {
        assert(true, s"Only three data types, SInt, UInt and AFix are supported")
      }
    }
    res.asInstanceOf[T]
  }
  // Abs
  def abs[T <: Data with Num[T]](source: T): T = {
    val res = source match {
      case source: UInt => source
      case source: SInt => {
        val abs = source.asInstanceOf[SInt].absWithSym
        val toSInt = S(0,1 bits) @@ abs.asSInt
        toSInt
      }
      case source: AFix =>{
        if(source.asInstanceOf[AFix].signed){
          val maxRaw = source.asInstanceOf[AFix].maxRaw
          val minRaw = source.asInstanceOf[AFix].minRaw
          val expNumber = source.asInstanceOf[AFix].exp
          var tmp = new AFix(maxRaw, minRaw, expNumber)
          tmp.raw := ~source.asInstanceOf[AFix].raw
          val bias = new AFix(maxRaw, minRaw, expNumber)
          bias := math.pow(2, expNumber)
          val max = new AFix(maxRaw, minRaw, expNumber)
          max := source.asInstanceOf[AFix].maxValue.toDouble
          tmp = tmp +| bias
          Mux(source.isNegative(), Mux(!tmp.raw.msb, tmp, max), source)
        } else {
          source
        }
      }
      case _ => {
        assert(true, s"Only three data types, SInt, UInt and AFix are supported")
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