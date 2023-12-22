package Eyeriss.Ctrl

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb._
import spinal.lib.bus.regif.Apb3BusInterface


/* using the apb to drive some regs in the eyeriss */

class CSB extends Component{

  val io = new Bundle{
    val bus = slave (Apb3(32,32))
  }

  val interface = Apb3SlaveFactory(io.bus)

  noIoPrefix()
}
