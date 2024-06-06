package BetsyLibs

/**
 ** Betsy follow the MiT Licence.(c) xxl, All rights reserved **
 ** Update Time : 2024/4/19      SpinalHDL Version: 1.94       **
 ** You should have received a copy of the MIT License along with this library **
 ** Test Status : PASS :)      Version:0.1 **
 */

import Betsy.Until.{BetsyModule, zero}
import spinal.core._
import spinal.lib._

//Todo SRAM lib Support later...

trait MemoryKind
object RegistersBank extends MemoryKind
object SpinalMem extends MemoryKind
object BlockRAM  extends MemoryKind
object SRAM extends MemoryKind

/* no mask usage if maskWidth == -1 ...*/
case class InnerPort[T<:Data](gen:HardType[T],depth:Long,maskWidth:Int = -1) extends Bundle with IMasterSlave {
  val address = UInt(log2Up(depth) bits)
  val ren = Bool()
  val rdata = gen()
  val wen = Bool()
  val wdata = gen()
  val wmask = ifGen(maskWidth != -1){Bits(maskWidth bits)}
  override def asMaster(): Unit = {
    out(address,wdata,wen,ren,wmask)
    in(rdata)
  }
}

class MemoryImpl[T <: Data](gen:HardType[T],depth:Long,ports:Int,
                            impl:MemoryKind,maskWidth:Int = -1,initContent:Array[BigInt] = null) extends BetsyModule{
  val io = new Bundle{
    val Ports = Vec(slave(InnerPort(gen,depth,maskWidth)),ports)
  }
  require(ports > 0 && ports <= 2 && depth < Int.MaxValue,"ports is illegal in the memory !!!")
  ports match {
    case 1 => assert(!io.Ports(0).ren && !io.Ports(0).wen,"undefined behavior in single-port SRAM")
    case 2 => assert(!(io.Ports(0).ren && io.Ports(0).wen && io.Ports(1).ren && io.Ports(1).wen && io.Ports(0).address === io.Ports(1).address),"undefined behavior in dual-ported SRAM")
    case _ =>
  }

  impl match {
    case `RegistersBank` => { //when wen and ren at same time -> read first
      val mem = Vec(Reg(gen()).init(zero(gen())),depth.toInt)
      for(idx <- 0 until ports) yield {
        val rdata = Reg(gen()).init(zero(gen()))
        io.Ports(idx).rdata := rdata
        when(io.Ports(idx).wen) {
          when(io.Ports(idx).ren) {
            rdata := mem(io.Ports(idx).address)
          }.otherwise {
            mem(io.Ports(idx).address) := io.Ports(idx).wdata
          }
        }.otherwise {
          when(io.Ports(idx).ren){
            rdata := mem(io.Ports(idx).address)
          }
        }
      }
    }
    case `SpinalMem` => {
      val mem = Mem(gen,depth)
      if (initContent != null) { //just for the debug usage
        mem.initBigInt(initContent.toSeq)
      }
      for(idx <- 0 until ports){
        io.Ports(idx).rdata := mem.readSync(io.Ports(idx).address,enable = io.Ports(idx).ren)
        mem.write(io.Ports(idx).address,io.Ports(idx).wdata,enable = io.Ports(idx).wen,mask = io.Ports(idx).wmask)
      }
    }
    case `BlockRAM` => {
      val mem = Mem(gen, depth)
      if(initContent != null){ //just for the debug usage
        mem.initBigInt(initContent.toSeq)
      }
      mem.addAttribute("ram_style = \"block\"")
      for (idx <- 0 until ports) {
        io.Ports(idx).rdata := mem.readSync(io.Ports(idx).address, enable = io.Ports(idx).ren)
        mem.write(io.Ports(idx).address, io.Ports(idx).wdata, enable = io.Ports(idx).wen, mask = io.Ports(idx).wmask)
      }
    }
    case _ => /* black box for sram */
  }
}