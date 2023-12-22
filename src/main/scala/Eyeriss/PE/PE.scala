package Eyeriss.PE

import spinal.core._
import spinal.lib._
import Eyeriss._


/**
 * the calculate unit in the eyeriss accelerator
 * the MAC unit of the pe
 */


case class PEConfig(
                   filterScratchPadDepth:Int = 224,
                   ifmapScratchPadDepth:Int = 16,
                   psumScratchPadDepth:Int = 24,
                   pipeMul:Boolean = true,
                   stride:Int= 1  // support the stride == 1 only
                   )


class PE(p:EyerissParameters,c:PEConfig) extends Component {

  import p._

  val io = new Bundle{
    val ifmap = slave Stream(SInt(DataWidth bits))
    val filter = slave Stream(Vec(SInt(DataWidth bits),4))
  }
  noIoPrefix()

  val filterScratch = Mem(SInt(DataWidth bits),c.filterScratchPadDepth)
  val ifmapScratch = RegInit(Vec(SInt(DataWidth bits),c.ifmapScratchPadDepth)).init(0)
  val psumScratch = RegInit(Vec(SInt(DataWidth bits),c.psumScratchPadDepth)).init(0)

  when(io.filter.fire){
    // filterScratch.write()
  }


}
