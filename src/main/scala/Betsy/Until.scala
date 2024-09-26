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
  def constConvert[T <: Data](gen:T, value:Int):T = {
    val const = gen match {
      case _: UInt => U(value, gen.getBitsWidth bits)
      case _: SInt => S(value, gen.getBitsWidth bits)
      case _: AFix => AF(BigDecimal(value), BitCount(gen.asInstanceOf[AFix].intWidth), BitCount(gen.asInstanceOf[AFix].fracWidth), gen.asInstanceOf[AFix].signed)
      case _: Bits => B(value, gen.getBitsWidth bits)
      case _ => throw new Exception("constConvert : not support the data type!!!")
    }
    const.asInstanceOf[T]
  }

  // abandon the clip and upDown
  // make into the Operations
  // Todo
  def resizePoint[T <: Data](gen:T, value: T):T = {
    val value_width = gen.getBitsWidth
    val resized = gen match {
      case _:UInt => value.asInstanceOf[UInt].resized
      case _:SInt => value.asInstanceOf[SInt].resized
      case _:AFix => value.asInstanceOf[AFix].truncated
    }
    resized.asInstanceOf[T]
  }
  /***************** Arithmetic operations function ends *****************/
}

