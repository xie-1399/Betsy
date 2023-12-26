package Eyeriss.Buffer

import spinal.core._
import spinal.lib._
import Eyeriss._
import Utils._

/**
 *
 * the fifo is made by the regs and no flush and no other complex logic
 */

class FIFOGroup(p:EyerissParameters,c:FIFOGroup.FIFOParameters) extends Component{
   import p._
   import c._
  /* a group fifo is set here */
  val io  = new Bundle{
    val ifMapIn = slave Stream(SInt(DataWidth bits))
    val filterIn = slave Stream(SInt(64 bits))
    val pSumIn = slave Stream(SInt(64 bits))

    val ifMapOut = master Stream (SInt(DataWidth bits))
    val filterOut = master Stream (SInt(64 bits))
    val pSumOut = master Stream (SInt(64 bits))
  }

  /* three fifo groups without flush */
  val ifmapFIFO = FIFO(SInt(DataWidth bits),c.ifmapDepth)
  val filterFIFO = FIFO(SInt(64 bits),c.filterDepth)
  val psumFIFO = FIFO(SInt(64 bits),c.PsumDepth)

  io.ifMapIn >> ifmapFIFO.io.enqueue
  ifmapFIFO.io.dequeue >> io.ifMapOut

  io.filterIn >> filterFIFO.io.enqueue
  filterFIFO.io.dequeue >> io.filterOut

  io.pSumIn >> psumFIFO.io.enqueue
  psumFIFO.io.dequeue >> io.pSumOut
}

object FIFOGroup{
  /* if config as the constant */
  case class FIFOParameters(
                             ifmapDepth: Int = 16,
                             filterDepth: Int = 16,
                             PsumDepth: Int = 16
                           )
  def main(args: Array[String]): Unit = {
    SpinalVerilog(new FIFOGroup(EyerissParameters(),FIFOParameters()))
  }

}