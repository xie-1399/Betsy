package Betsy
/**
 ** Betsy follow the MiT Licence.(c) xxl, All rights reserved **
 ** Update Time : 2024/9/24      SpinalHDL Version: 1.94       **
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
      case _: SFix => SF(BigDecimal(0), ExpNumber(gen.asInstanceOf[SFix].maxExp), BitCount(gen.asInstanceOf[SFix].bitCount))
      case _: UFix => UF(BigDecimal(0), ExpNumber(gen.asInstanceOf[UFix].maxExp), BitCount(gen.asInstanceOf[UFix].bitCount))
      case _: Bits => B(0, gen.getBitsWidth bits)
      case _ => throw new Exception("only support {UInt,SInt,SFix,UFix} 4 dataTypes")
    }
    zero.asInstanceOf[T]
  }

  /* all kinds type of one value */
  def one[T <: Data](gen: T): T = {
    val one = gen match {
      case _: UInt => U(1, gen.getBitsWidth bits)
      case _: SInt => S(1, gen.getBitsWidth bits)
      case _: SFix => SF(BigDecimal(1), ExpNumber(gen.asInstanceOf[SFix].maxExp), BitCount(gen.asInstanceOf[SFix].bitCount))
      case _: UFix => UF(BigDecimal(1), ExpNumber(gen.asInstanceOf[UFix].maxExp), BitCount(gen.asInstanceOf[UFix].bitCount))
      case _ => throw new Exception("only support {UInt,SInt,SFix,UFix} 4 dataTypes")
    }
    one.asInstanceOf[T]
  }

  def min[T <: Data](gen: T): BigInt = {
    val min = gen match {
      case _: UInt => gen.asInstanceOf[UInt].minValue.toBigInt
      case _: SInt => gen.asInstanceOf[SInt].minValue.toBigInt
      case _: SFix => gen.asInstanceOf[SFix].minValue.toBigInt
      case _: UFix => gen.asInstanceOf[UFix].minValue.toBigInt
      case _ => throw new Exception("only support {UInt,SInt,SFix,UFix} 4 dataTypes")
    }
    min
  }

  def max[T <: Data](gen: T): BigInt = {
    val max = gen match {
      case _: UInt => gen.asInstanceOf[UInt].maxValue.toBigInt
      case _: SInt => gen.asInstanceOf[SInt].maxValue.toBigInt
      case _: SFix => gen.asInstanceOf[SFix].maxValue.toBigInt
      case _: UFix => gen.asInstanceOf[UFix].maxValue.toBigInt
      case _ => throw new Exception("only support {UInt,SInt,SFix,UFix} 4 dataTypes")
    }
    max
  }

  def clip[T <: Data](value: T, max: BigInt, min: BigInt): T = {

    val clipNum = value match {
      case _: UInt => Mux(value.asInstanceOf[UInt] > U(max), U(max), Mux(value.asInstanceOf[UInt] < U(min), U(min), value))
      case _: SInt => Mux(value.asInstanceOf[SInt] > S(max), S(max), Mux(value.asInstanceOf[SInt] < S(min), S(min), value))
      case _: SFix => {
        val SF_max = SF(BigDecimal(max), ExpNumber(value.asInstanceOf[SFix].maxExp), BitCount(value.asInstanceOf[SFix].bitCount))
        val SF_min = SF(BigDecimal(min), ExpNumber(value.asInstanceOf[SFix].maxExp), BitCount(value.asInstanceOf[SFix].bitCount))
        Mux(value.asInstanceOf[SFix] > SF_max, SF_max, Mux(value.asInstanceOf[SFix] < SF_min, SF_min, value))
      }
      case _: UFix => {
        val UF_max = UF(BigDecimal(max), ExpNumber(value.asInstanceOf[UFix].maxExp), BitCount(value.asInstanceOf[UFix].bitCount))
        val UF_min = UF(BigDecimal(min), ExpNumber(value.asInstanceOf[UFix].maxExp), BitCount(value.asInstanceOf[UFix].bitCount))
        Mux(value.asInstanceOf[UFix] > UF_max, UF_max, Mux(value.asInstanceOf[UFix] < UF_min, UF_min, value))
      }
      case _ => value
    }
    clipNum.asInstanceOf[T]
  }

  def upDown[T <: Data](value: T, gen: T): T = {
    val maxValue = max(gen)
    val minValue = min(gen)
    val upDownValue = clip(value, maxValue, minValue)
    upDownValue
  }

  def resizePoint[T <: Data](value: T, gen:T):T = {
    val resized = gen match {
      case _:UInt => value.resized
      case _:SInt => value.resized
      case _:SFix => value.asInstanceOf[SFix].truncated
      case _:UFix => value.asInstanceOf[UFix].truncated
    }
    resized.asInstanceOf[T]
  }

  /***************** Arithmetic operations function ends *****************/

}

