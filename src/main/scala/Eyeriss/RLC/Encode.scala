package Eyeriss.RLC

import Eyeriss.EyerissParameters
import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

/* compress the data into the RLC format */

case class rlcFormat(p:EyerissParameters) extends Bundle{
  val run = UInt(5 bits)
  val level = Bits(16 bits)
}

class Encode(config:EyerissParameters) extends Component {

  val io = new Bundle{
    val compress = out Bool()
    val dataIn = slave Stream(Bits(config.DataWidth bits))
    val dataOut = master Stream(Bits(config.RLCDataWidth bits))
    val error = out Bool()
  }

  val ready = RegInit(False).setWhen(io.dataIn.valid)
  val codes = Vec(Reg(rlcFormat(config)),3)
  val error = RegInit(False)

  io.compress := False
  io.dataOut.valid := False
  io.dataOut.payload := 0
  io.dataIn.ready := ready
  io.error := error

  val EncoderFsm = new StateMachine{

    val runCounter = Counter(32)
    val iter  = Counter(0,3)
    val Idle:State = new State with EntryPoint {
        whenIsActive{
          when(io.dataIn.valid){
            when(io.dataIn.payload.asUInt =/= 0){error := True}
            goto(Run)
          }
        }
    }

    val Run:State = new State{
      whenIsActive{
        when(io.dataIn.fire){
          when(io.dataIn.payload.asUInt === 0) {
            runCounter.increment()
          }.otherwise{
            codes(iter.value).run := runCounter.value
            codes(iter.value).level := io.dataIn.payload
            goto(Level)
          }
        }
      }
    }

    val Level:State = new State{
      whenIsActive{
        iter.increment()
        when(iter.valueNext === 3){
          goto(Finish)
        }.otherwise{
          runCounter.value := 1
          goto(Run)
        }
      }
    }

    val Finish:State = new State{
      whenIsActive{
        io.dataOut.valid := True
        io.dataOut.payload := B"0" ## codes.asBits
        io.compress := True
        ready := False
        runCounter.clear()
        iter.clear()
        when(io.dataOut.fire) {
          goto(Idle)
        }
      }
    }
  }

}

object Encode extends App{
  val config = EyerissParameters()
  val withString = true
  if(!withString){
    SpinalConfig(enumPrefixEnable = false).withoutEnumString().generateVerilog(new Encode(config))
  }
  else{
    SpinalVerilog(new Encode(config))
  }
}
