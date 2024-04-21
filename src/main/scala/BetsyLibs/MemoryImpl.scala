package BetsyLibs

/**
 ** Betsy follow the MiT Licence.(c) xxl, All rights reserved **
 ** Update Time : 2024/4/19      SpinalHDL Version: 1.94       **
 ** You should have received a copy of the MIT License along with this library **
 ** **
 */

import Betsy.Until.{BetsyModule, zero}
import spinal.core._
import spinal.lib._

//Todo add Bram and SRAM later...

class MemoryKind extends SpinalEnum{
  val RegistersBank,SpinalMem,SRAM,BlockRAM = newElement()
  defaultEncoding = SpinalEnumEncoding("MemoryImpl")(
    RegistersBank -> 0,  // Regs
    SpinalMem -> 1,    // Mem
    BlockRAM -> 2,    // FPGA
    SRAM -> 3        // ASIC
  )
}

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

class SinglePortSyncMem[T <: Data](depth:Int,dataType:HardType[T],kind: MemoryKind,maskWidth:Int = -1) extends BetsyModule{
  /* the single (read or write behavior) at the same time / so each time is read or write */
  val io = new Bundle{
    val memIo = slave(SinglePortedSyncMemIO(depth,dataType,maskWidth))
  }
  assert(!io.memIo.ren && !io.memIo.wen,"undefined behavior in single-port SRAM")
  kind match {
    case kind.RegistersBank => {
      val mem = Vec(Reg(dataType).init(zero(dataType())),depth)
      val output = RegInit(zero(gen = dataType()))
      io.memIo.rdata := output
      when(io.memIo.wen){
        
      }
    }

    case kind.SpinalMem => {
      val mem = Mem(dataType,depth)
      mem.write(io.memIo.addr,io.memIo.wdata,enable = io.memIo.wen,mask = io.memIo.mask)
      io.memIo.rdata := mem.readSync(io.memIo.addr,enable = io.memIo.ren)
    }
    case _ =>
  }


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


object MemoryImpl extends App{

}