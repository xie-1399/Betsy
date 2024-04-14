package Betsy

import spinal.core._
import spinal.lib._
import Until._

/**
 ** Betsy follow the MiT Licence.(c) xxl, All rights reserved **
 ** Update Time : 2024/4/6      SpinalHDL Version: 1.94       **
 ** You should have received a copy of the MIT License along with this library **
 ** **
 */

case class PESignals[T <: Data](inputType:HardType[T],outputType:HardType[T],accType:HardType[T],max_matmuls:Int) extends Bundle{
  /* signals in the PE component */

  val in_id = UInt(log2Up(max_matmuls)bits)
  val out_id = UInt(log2Up(max_matmuls) bits)



  val bad_dataflow = Bool()


}


class PE[T <: Data](inputType:HardType[T],outputType:HardType[T],accType:HardType[T],max_matmuls:Int) extends BetsyModule{
  val io = new Bundle{



  }

}
