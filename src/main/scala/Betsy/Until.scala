package Betsy
/**
 ** Betsy follow the MiT Licence.(c) xxl, All rights reserved **
 ** Update Time : 2024/4/6      SpinalHDL Version: 1.94       **
 ** You should have received a copy of the MIT License along with this library **
 */

import spinal.core._
import spinal.lib._

object Until{

  /* BetsyModule no need for the IO prefix */
  class BetsyModule() extends Component{
    noIoPrefix()
  }

  trait Size{
    val size:UInt
  }

  trait Stride {
    val stride: UInt
  }

  trait Address {
    val address: UInt
  }

  trait Reverse{
    val reverse:Bool
  }

  case class BetsyFlow[T <: Data](flow:HardType[T]) extends Bundle{
    val transFlow = Flow(flow)

    def push(b: T): Unit = transFlow.push(b) /* set valid and push the data in the flow */
    def pop():T = {
      transFlow.valid := False
      transFlow.payload
    }
  }

  case class withLast[T <: Data](gen: HardType[T]) extends Bundle {
    /* wrapper the bundle with last signal */
    val last = out Bool()
    val payload = out(gen())
  }


  def Demux[T <: Data](cond: Bool, con: T, alt: T): T = {
    val input = cloneOf(con)
    when(cond) {
      con := input
    }.otherwise {
      alt := input
    }
    input
  }

  /***************** Arithmetic operations function *****************/
  /* if overflow the add max , will from the start to calculate */
  def wrappingAdd(s1:UInt,s2:UInt,overflow:Int):UInt = {
    require(overflow > 0,"overflow must great than 0!!!")
    val max = overflow - 1
    val width = log2Up(overflow + 1)
    val plus = s1 + s2  /* not with carry */
    val res = Mux(plus.resize(width) > U(overflow),plus - max - 1,plus) /* keep the bit width */
    res
  }

  /* all kinds type of zero value */
  def zero[T <: Data](gen: T): T = {
    val zero = gen match {
      case _: UInt => U(0, gen.getBitsWidth bits)
      case _: SInt => S(0, gen.getBitsWidth bits)
      case _: SFix => SF(BigDecimal(0), ExpNumber(gen.asInstanceOf[SFix].maxExp), BitCount(gen.asInstanceOf[SFix].bitCount)) /* convert the fixed float */
      case _: Bits => B(0, gen.getBitsWidth bits)
      case _ => B(0, gen.getBitsWidth bits)
    }
    zero.asInstanceOf[T]
  }

  /* all kinds type of one value */
  def one[T <: Data](gen: T): T = {
    val one = gen match {
      case _: UInt => U(1, gen.getBitsWidth bits)
      case _: SInt => S(1, gen.getBitsWidth bits)
      case _: SFix => SF(BigDecimal(1), ExpNumber(gen.asInstanceOf[SFix].maxExp), BitCount(gen.asInstanceOf[SFix].bitCount))
      case _ => U(1, gen.getBitsWidth bits)
    }
    one.asInstanceOf[T]
  }

  def min[T <: Data](gen: T): BigInt = {
    val min = gen match {
      case _: UInt => BigInt(0)
      case _: SInt => gen.asInstanceOf[SInt].minValue.toBigInt
      case _: SFix => gen.asInstanceOf[SFix].minValue.toBigInt
      case _ => BigInt(0)
    }
    min
  }

  def max[T <: Data](gen: T): BigInt = {
    val max = gen match {
      case _: UInt => gen.asInstanceOf[UInt].maxValue.toBigInt
      case _: SInt => gen.asInstanceOf[SInt].maxValue.toBigInt
      case _: SFix => gen.asInstanceOf[SFix].maxValue.toBigInt
      case _ => gen.asInstanceOf[UInt].maxValue.toBigInt
    }
    max
  }

  def clip[T <: Data with Num[T]](value: T, max: BigInt, min: BigInt): T = {
    val clipNum = value match {
      case _: UInt => Mux(value.asInstanceOf[UInt] > U(max), U(max), Mux(value.asInstanceOf[UInt] < min, U(min), value))
      case _: SInt => Mux(value.asInstanceOf[SInt] > S(max), S(max), Mux(value.asInstanceOf[SInt] < min, S(min), value))
      case _ => value
    }
    clipNum.asInstanceOf[T]
  }

  def mac[T <: Data with Num[T]](gen: T, m1: T, m2: T, acc: T): T = {
    // require(getType(gen) == getType(m1) == getType(m2) == getType(acc),"mac dataType not match !!!")
    val macValue = (gen, m1, m2, acc) match {
      // case (gen: SFix, x: SFix, y: SFix, z: SFix) => println("") Todo with the fixed float mac operation
      case _ => m1 * m2 + acc
    }
    macValue
  }

  def upDown[T <: Data with Num[T]](value: T, gen: T): T = {
    val maxValue = max(gen)
    val minValue = min(gen)
    val upDownValue = clip(value, maxValue, minValue)
    upDownValue
  }
  /***************** Arithmetic operations function ends *****************/

}

