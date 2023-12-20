package Eyeriss.RLC

import Eyeriss.Buffer._
import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axi._
import Eyeriss._

/**
 * the rlc unit will contain both the encoder and the decode
 */

object memorySim{
  /* using the axi4 memory sim to connect It */
  def getAxi4Config = Axi4Config(addressWidth = 32, dataWidth = 64, useId = false, useLock = false, useQos = false,
    useResp = false, useAllStrb = false, useProt = false, useCache = false, useBurst = false,
    useRegion = false, useSize = false, useLen = false, useStrb = false)
}


object DRAMConnect{
  def dramConfig = DRAMConfig()
}


class RLC(p:EyerissParameters) extends Component{

  val busConfig = if(p.UsingChipDRAM) DRAMConnect.dramConfig.getAxi4Config else memorySim.getAxi4Config

  val io = new Bundle{
    val busPort = master(Axi4(busConfig))
    val DecoderOut = master(Stream(Bits(p.DataWidth bits)))
    val EncoderIn = slave(Stream(Bits(p.DataWidth bits)))
    val error = out Bool()
  }

  val encode = new Encode(p)
  val decode = new Decode(p)

  decode.io.dataIn.valid := io.busPort.r.valid
  io.busPort.r.ready := decode.io.dataIn.ready && decode.io.end
  decode.io.dataIn.payload := io.busPort.r.payload.data
  decode.io.dataOut >> io.DecoderOut

  io.error := decode.io.error

  val DRAM = ifGen(p.UsingChipDRAM){
    new Area {
      val chipDram = new ChipDRAM(DRAMConnect.dramConfig)
      io.busPort <> chipDram.io.busPort
    }

  }

}
