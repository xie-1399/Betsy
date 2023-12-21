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

// Todo with the AXI about the read address and write address

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

  /* decode unit connect */
  decode.io.dataIn.valid := io.busPort.r.valid
  io.busPort.r.ready := decode.io.dataIn.ready && decode.io.end
  io.busPort.ar.valid := True
  io.busPort.ar.addr := 0

  decode.io.dataIn.payload := io.busPort.r.payload.data
  decode.io.dataOut >> io.DecoderOut

  io.error := decode.io.error || encode.io.error

  /* encode unit connect */
  encode.io.dataIn.connectFrom(io.EncoderIn)

  io.busPort.aw.valid := encode.io.dataOut.valid
  io.busPort.aw.addr := 0

  io.busPort.w.arbitrationFrom(encode.io.dataOut)
  io.busPort.w.data := encode.io.dataOut.payload
  io.busPort.w.last := True

  io.busPort.b.ready := True

  val DRAM = ifGen(p.UsingChipDRAM){
    new Area {
      val chipDram = new ChipDRAM(DRAMConnect.dramConfig)
      io.busPort <> chipDram.io.busPort
    }
  }

}

object RLC extends App{
  val config = EyerissParameters()
  val withString = true
  if (!withString) {
    SpinalConfig(enumPrefixEnable = false).withoutEnumString().generateVerilog(new RLC(config))
  }
  else {
    SpinalVerilog(new RLC(config))
  }
}