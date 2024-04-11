package Betsy

/**
 ** Betsy follow the MiT Licence.(c) xxl, All rights reserved **
 ** Update Time : 2024/4/6      SpinalHDL Version: 1.94       **
 ** You should have received a copy of the MIT License along with this library **
 ** the scratch pad banks **
 */

import spinal.core._
import spinal.lib._
import Betsy.Until._

case class ExtMemIO[T <: Data](dataType:HardType[T],depth:Int,maskWidth:Int) extends Bundle with IMasterSlave {
  def addrWidth = log2Up(depth)
  val ren = Bool()
  val raddr = UInt(addrWidth bits)
  val rdata = dataType()

  val wen = Bool()
  val waddr = UInt(addrWidth bits)
  val wdata = dataType()
  val wmask = Bits(maskWidth bits)
  override def asMaster(): Unit = {
    out(ren,raddr,wdata,wen,waddr,wmask)
    in(rdata)
  }
}

case class ExtSpadMemIO[T <: Data](dataType:HardType[T],depth:Int,maskWidth:Int,sp_banks:Int) extends Bundle{
  val spad = Vec(ExtMemIO(dataType,depth,maskWidth),sp_banks)
}

class SharedExtMem(sp_banks:Int,sp_depth:Int,sp_maskWdith:Int) extends BetsyModule {
  val nShares = 2
  val io = new Bundle{
    // val
  }

  // val spadMem = Mem()

  val SyncRead = new Area {


  }



}
