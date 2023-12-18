package Eyeriss.Buffer

import spinal.core._
import spinal.lib._
import Eyeriss._


/**
 *
 * the fifo is made by the regs
 */

class FIFO(p:EyerissParameters) extends Component {
  val io = new Bundle{
    val push = slave Stream(Bits(p.DataWidth bits))
    val pop = master Stream(Bits(p.DataWidth bits))
  }
  val InterFifo = StreamFifo(Bits(p.DataWidth bits),p.FIFODepth) /* the latency of the fofo is 2 */
  InterFifo.io.push <> io.push
  InterFifo.io.pop <> io.pop
}

class FIFOGroup() extends Component{
 /* a group fifo is set here */

}