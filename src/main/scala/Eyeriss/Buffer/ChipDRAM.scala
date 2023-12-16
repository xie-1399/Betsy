package Eyeriss.Buffer

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axi._

trait Arch
object ASIC extends Arch
object Memory extends Arch


case class DRAMConfig(
                     DRAMSize:BigInt = 128 MiB,
                     arch: Arch = Memory,
                     dataWidth:Int = 64,
                     ) {
  def addrWidth = log2Up(DRAMSize / 8)
  def totalBytes = DRAMSize / 8

  def getAxi4Config = Axi4Config(addressWidth = addrWidth,dataWidth = dataWidth,useId = false,useLock = false,useQos = false,
    useResp = false,useAllStrb = false,useProt = false,useCache = false)
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

  val DRAM = Mem(Bits(8 bits),c.addrWidth)

  val rspAddr = RegNextWhen(io.busPort.ar.addr,fire)

  val fire = RegInit(False).setWhen(io.busPort.ar.fire).clearWhen(io.rsp.data.valid)
  //val burstCounter = Counter(0,)

  val rspData = DRAM.readAsync(rspAddr)

  io.busPort.ar.ready := True
  io.busPort.ar.setBurstINCR()
  io.busPort.ar.len := c.dataWidth / 8 - 1

  io.busPort.r.valid := RegNext(io.busPort.ar.fire)
  io.busPort.r.payload.data := rspData
  io.busPort.r.last := True

  io.busPort.aw.ready := True

  io.rsp.data.valid := io.busPort.r.valid
  io.rsp.data.payload := io.busPort.r.payload.data
}

/* using the chip dram with the axi4 bus port */

object ChipDRAM extends App{
  SpinalVerilog(new ChipDRAM(DRAMConfig()))
}