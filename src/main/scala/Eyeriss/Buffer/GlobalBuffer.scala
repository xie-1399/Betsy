package Eyeriss.Buffer

/* here set a global buffer with 108 KB */

import spinal.core._
import spinal.lib._


case class BufferParameters(
                           BufferSize:BigInt = 108 KiB,
                           )

class GlobalBuffer extends Component {
  
  val io = new Bundle{

  }

}
