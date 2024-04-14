package Sloan

/**
 ** Sloan follow the MiT Licence.(c) xxl, All rights reserved **
 ** Update Time : 2024/4/13      SpinalHDL Version: 1.94       **
 ** You should have received a copy of the MIT License along with this library **
 ** common until in the Sloan accelerator **
 */

import spinal.core._
import spinal.lib._
import scala.reflect.runtime.universe._

object Until {
  /* delete the IO prefix*/
  class SloanModule() extends Component {
    noIoPrefix()
  }

  case class withLast[T <: Data](gen:HardType[T]) extends Bundle{
    /* wrapper the bundle with last signal */
    val last = out Bool()
    val payload = out(gen())
  }

  /* all kinds type of zero value */
  def zero[T <: Data](gen: T):T = {
    val zero = gen match {
      case _: UInt => U(0, gen.getBitsWidth bits)
      case _: SInt => S(0, gen.getBitsWidth bits)
      case _: SFix => SF(BigDecimal(0), ExpNumber(gen.asInstanceOf[SFix].maxExp), BitCount(gen.asInstanceOf[SFix].bitCount)) /* convert the fixed float */
      case _ => U(0, gen.getBitsWidth bits)
    }
    zero.asInstanceOf[T]
  }

  /* all kinds type of one value */
  def one[T <: Data](gen: T):T = {
    val one = gen match {
      case _: UInt => U(1, gen.getBitsWidth bits)
      case _: SInt => S(1, gen.getBitsWidth bits)
      case _: SFix => SF(BigDecimal(1), ExpNumber(gen.asInstanceOf[SFix].maxExp), BitCount(gen.asInstanceOf[SFix].bitCount))
      case _ => U(1, gen.getBitsWidth bits)
    }
    one.asInstanceOf[T]
  }

  def getType[T: TypeTag](obj: T): Type = {
    typeOf[T]
  }

  def mac[T <: Data with Num[T] ](gen:T,m1:T,m2:T,acc:T) = {
    // require(getType(gen) == getType(m1) == getType(m2) == getType(acc),"mac dataType not match !!!")
    val macValue = (gen,m1,m2,acc) match {
      // case (gen: SFix, x: SFix, y: SFix, z: SFix) => println("") Todo with the fixed float mac operation
      case _ => m1 * m2 + acc
    }
    macValue
  }

}
