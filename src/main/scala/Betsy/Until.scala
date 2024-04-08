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

  case class BetsyFlow[T <: Data](flow:HardType[T]) extends Bundle{
    val transFlow = Flow(flow)
    def push(b:T) = transFlow.push(b) /* set valid and push the data in the flow */
    def pop():T = {
      transFlow.valid := False
      transFlow.payload
    }
  }

  /* if overflow the add max , will from the start to calculate */
  def wrappingAdd(s1:UInt,s2:UInt,overflow:Int):UInt = {
    require(overflow > 0,"overflow must great than 0!!!")
    val max = overflow - 1
    val width = log2Up(overflow + 1)
    val plus = s1 + s2  /* not with carry */
    val res = Mux(plus.resize(width) > U(overflow),plus - max - 1,plus) /* keep the bit width */
    res
  }

  /* popCount function in chisel instead (get one numbers)*/
  def popCount = CountOne




}

