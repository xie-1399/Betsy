package Eyeriss.Buffer

/* here set a global buffer with 108 KB */

import spinal.core._
import spinal.lib._


/* using the one single port to read and write */

case class BufferParameters(
                           BufferSize:BigInt = 108 KiB,
                           ){
  val filterBanks = 2
  val filterSize = 4 KiB
  val ifMapAndPsumBanks = 25
  val mapBankSize = 4 KiB
  // require((filterBanks * filterSize) + (mapBankSize * ifMapAndPsumBanks) == BufferSize)
}

class GlobalBuffer(bufferParameters: BufferParameters) extends Component {
  import bufferParameters._

  val io = new Bundle{
  }

  val filterRegion = for(idx <- 0 until filterBanks) yield new Area {
    val filterBank = Mem(Bits(64 bits),512)
  }
  val fpRegion = for(idx <- 0 until ifMapAndPsumBanks) yield new Area {
    val fpBanks = Mem(Bits(64 bits),512)
  }

}
