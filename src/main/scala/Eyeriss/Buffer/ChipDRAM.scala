package Eyeriss.Buffer

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axi._
import spinal.core.sim._

trait Arch
object ASIC extends Arch
object Memory extends Arch

trait Bus
object APB extends Bus
object AXI extends Bus

case class DRAMConfig(
                     DRAMSize:BigInt = 128 MiB,
                     arch: Arch = Memory,
                     dataWidth:Int = 64,
                     align:Boolean = true,
                     bus:Bus = AXI,
                     whiteBox:Boolean = true
                     ) {
  def addrWidth = log2Up(DRAMSize / 8)
  def totalBytes = DRAMSize / 8

  def getAxi4Config = Axi4Config(addressWidth = addrWidth,dataWidth = dataWidth,useId = false,useLock = false,useQos = false,
    useResp = false,useAllStrb = false,useProt = false,useCache = false,useBurst = false,useRegion = false,useSize = false,useLen = false,useStrb = false)
}

/* only fetch the DRAM data bits */
case class Rsp(c:DRAMConfig) extends Bundle with IMasterSlave {
  val data = Flow (Bits(c.dataWidth bits))
  override def asMaster(): Unit = {
    master(data)
  }
}

class ChipDRAM(c:DRAMConfig) extends Component {

  val io = new Bundle{
    val busPort = slave (Axi4(DRAMConfig().getAxi4Config))
    val rsp = master (Rsp(c))
  }
  noIoPrefix()

  val DRAM = Mem(Bits(8 bits),c.totalBytes)
  val addr = if(c.align) io.busPort.ar.addr(c.addrWidth - 1 downto 3) @@ U(0,3 bits) else io.busPort.ar.addr

  val writeAddr = RegNextWhen(io.busPort.aw.addr,io.busPort.aw.fire)

  val rspData = (0 until c.dataWidth/8).map{
    offset =>
      DRAM.readSync(io.busPort.ar.addr + offset,enable = io.busPort.ar.fire)
  }

  val write = (0 until c.dataWidth/8).map{
    offset =>
      // here arrange the memory order
      DRAM.write(writeAddr + offset,io.busPort.w.data(c.dataWidth - 1 - (8 * offset) downto 8 * (c.dataWidth / 8 - offset - 1)),enable = io.busPort.w.fire)
  }


  /* reading happens */
  io.busPort.ar.ready := True
  io.busPort.r.valid := RegNext(io.busPort.ar.fire)
  io.busPort.r.payload.data := rspData.reduceBalancedTree(_ ## _)
  io.busPort.r.last := True

  /* writing happens */
  io.busPort.aw.ready := True
  io.busPort.w.ready := True
  io.busPort.b.valid := RegNext(io.busPort.w.fire)

  io.rsp.data.valid := io.busPort.r.valid
  io.rsp.data.payload := io.busPort.r.payload.data


  val whiteBox = c.whiteBox generate new Area{
    DRAM.simPublic()
  }

}

/* using the chip dram with the axi4 bus port */

object ChipDRAM extends App{
  SpinalVerilog(new ChipDRAM(DRAMConfig()))
}