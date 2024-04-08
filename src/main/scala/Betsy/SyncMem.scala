package Betsy
/**
 ** Betsy follow the MiT Licence.(c) xxl, All rights reserved **
 ** Update Time : 2024/4/7      SpinalHDL Version: 1.94       **
 ** You should have received a copy of the MIT License along with this library **
 ** defines some ports and operations about the sync memory **
 */

import spinal.core._
import spinal.lib._
import Betsy.Until._

case class SinglePortedSyncMemIO[T <: Data](depth:Int,dataType:HardType[T],maskWidth:Int = -1) extends Bundle with IMasterSlave {
  val addr = UInt(log2Up(depth) max 1 bits)
  val wdata = dataType()
  val rdata = dataType()
  val wen = Bool()
  val ren = Bool()
  val mask = ifGen(maskWidth != -1) {Bits(maskWidth bits)}
  override def asMaster(): Unit = {
    out(addr,wdata,wen,ren,mask)
    in(rdata)
  }
}

case class TwoPortedSyncMemIO[T <: Data](depth:Int,dataType:HardType[T],maskWidth:Int = -1) extends Bundle with IMasterSlave {
  val waddr = UInt(log2Up(depth) max 1 bits)
  val raddr = UInt(log2Up(depth) max 1 bits)
  val wdata = dataType()
  val rdata = dataType()
  val wen = Bool()
  val ren = Bool()
  val mask = ifGen(maskWidth != -1) {Bits(maskWidth bits)}
  override def asMaster(): Unit = {
    out(waddr,raddr,wdata,wen,ren,mask)
    in(rdata)
  }
}

class SinglePortSyncMem[T <: Data](depth:Int,dataType:HardType[T],maskWidth:Int = -1) extends BetsyModule{
  /* the single (read or write behavior) at the same time / so each time is read or write */
  val io = new Bundle{
    val memIo = slave(SinglePortedSyncMemIO(depth,dataType,maskWidth))
  }
  assert(!io.memIo.ren && !io.memIo.wen,"undefined behavior in single-port SRAM")
  val mem = Mem(dataType,depth)
  mem.write(io.memIo.addr,io.memIo.wdata,enable = io.memIo.wen,mask = io.memIo.mask)
  io.memIo.rdata := mem.readSync(io.memIo.addr,enable = io.memIo.ren)
}

class TwoPortSyncMem[T <: Data](depth:Int,dataType:HardType[T],maskWidth:Int) extends BetsyModule{
  /* the two port sram (one for the read and one for the write ) */
  val io = new Bundle{
    val memIo = slave(TwoPortedSyncMemIO(depth,dataType,maskWidth))
  }
  assert(!(io.memIo.ren && io.memIo.wen && io.memIo.waddr === io.memIo.raddr),"undefined behavior in two-port SRAM")
  val mem = Mem(dataType,depth)
  mem.write(io.memIo.waddr,io.memIo.wdata,enable = io.memIo.wen,mask = io.memIo.mask)
  io.memIo.rdata := mem.readSync(io.memIo.raddr,enable = io.memIo.ren)
}

// Todo add using spilt the single port

object SinglePortSyncMem{
  def apply[T <: Data](depth:Int,dataType:HardType[T], maskWidth:Int):SinglePortSyncMem[T] = {
    val singlePortMem = new SinglePortSyncMem(depth,dataType,maskWidth)
    singlePortMem
  }
}

object TwoPortedSyncMem{
  def apply[T <: Data](depth: Int, dataType: HardType[T], maskWidth:Int): TwoPortSyncMem[T] = {
    val twoPortMem = new TwoPortSyncMem(depth, dataType, maskWidth)
    twoPortMem
  }
}

object SyncMemUsage extends App{
  val singlePortMem = SpinalSystemVerilog(new SinglePortSyncMem(64,Bits(32 bits)))
  val twoPortMem = SpinalSystemVerilog(new TwoPortSyncMem(64,Bits(32 bits),-1))
}