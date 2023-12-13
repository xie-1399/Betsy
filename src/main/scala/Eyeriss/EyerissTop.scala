package Eyeriss

import spinal.core._
import spinal.lib._


trait dataType
object UInt16 extends dataType
object SInt16 extends dataType

case class EyerissParameters(
                            DataType:dataType = UInt16,
                            RLCDataWidth:Int = 64,
                            RLCAddrWidth:Int = 16
                            ){
  require(DataType == UInt16 || DataType == SInt16)
  def DataWidth = DataType match {
    case `UInt16` => 16
    case `SInt16` => 16
  }
}

class EyerissTop(p:EyerissParameters) extends Component{

  val io = new Bundle{
    val clk = in Bool()
    val rst = in Bool()
  }



}
