package Eyeriss.Buffer

/* here set a global buffer with 108 KB */

import spinal.core._
import spinal.lib._


/* using the one single port to read and write */

case class BufferParameters(
                           BufferSize:BigInt = 108 KiB,
                           )

class GlobalBuffer(c: DRAMConfig) extends Component {
  
  val io = new Bundle{

  }

}
