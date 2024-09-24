package Betsy

/**
 ** Betsy follow the MiT Licence.(c) xxl, All rights reserved **
 ** Update Time : 2024/9/24      SpinalHDL Version: 1.94       **
 ** You should have received a copy of the MIT License along with this library **
 ** the alu and mac operations in the accelerator**
 */

import spinal.core._
import spinal.lib._

object Operations {

  // Add
  def add[T <: Data](gen:T, m1:T, m2:T): T = {
    val res = gen match {
      case _: SInt => m1.asInstanceOf[SInt] + m2.asInstanceOf[SInt]
      case _: UInt => m1.asInstanceOf[UInt] + m2.asInstanceOf[UInt]
      case _: SFix => m1.asInstanceOf[SFix] + m2.asInstanceOf[SFix]
      case _: UFix => m1.asInstanceOf[UFix] + m2.asInstanceOf[UFix]
      case _ => throw new Exception("only support {UInt,SInt,SFix,UFix} 4 dataTypes")
    }
    res.asInstanceOf[T]
  }


  // Sub

  // Mul

  // Divide

  // Increase

  // Decrease

  // Abs

  // greater than

  // greater than equal

  // less than

  // less than equal

  // MAC

  def mac[T <: Data](gen: T, m1: T, m2: T, acc: T): T = {
    // require(getType(gen) == getType(m1) == getType(m2) == getType(acc),"mac dataType not match !!!")
    // the mac unit will over the bit width of T
    val macValue = gen match {
      // case (gen: SFix, x: SFix, y: SFix, z: SFix) => println("")
      case _: SInt => m1.asInstanceOf[SInt] * m2.asInstanceOf[SInt] + acc.asInstanceOf[SInt]
      case _: UInt => m1.asInstanceOf[UInt] * m2.asInstanceOf[UInt] + acc.asInstanceOf[UInt]
      case _: SFix => m1.asInstanceOf[SFix] * m2.asInstanceOf[SFix] + acc.asInstanceOf[SFix]
      case _: UFix => m1.asInstanceOf[UFix] * m2.asInstanceOf[UFix] + acc.asInstanceOf[UFix]
      case _ => throw new Exception("only support {UInt,SInt,SFix,UFix} 4 dataTypes")
    }
    macValue.asInstanceOf[T]
  }

  // Min

  // Max
}
