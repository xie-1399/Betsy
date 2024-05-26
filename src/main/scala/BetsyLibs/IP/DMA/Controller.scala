package BetsyLibs.IP.DMA

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb.Apb3

/* the controller can be define with the bus if and reg if */

case class RegIf(controlDataWidth:Int) extends Bundle with IMasterSlave {
  val read = Bool()
  val write = Bool()
  val dataOut = Bits(controlDataWidth bits)
  val dataIn = Bits(controlDataWidth bits)

  override def asMaster(): Unit = {
    out(read,write,dataOut)
    in(dataIn)
  }
}


case class BusIf(controlRegCount: Int, controlDataWidth: Int) extends Bundle with IMasterSlave {
  val addr = UInt(log2Up(controlRegCount) bits)
  val dataOut = Bits(controlDataWidth bits)
  val dataIn = Bits(controlDataWidth bits)
  val read = Bool()
  val write = Bool()
  override def asMaster(): Unit = {
    out(addr,read,write,dataOut)
    in(dataIn)
  }
}


class Controller(config:DMAConfig) extends Component {

  val io = new Bundle{
    val bus = slave(BusIf(config.controlRegCount,config.controlDataWidth))
    // val regs =
  }

  /* add the bus interface bundle and reg interface*/

}
