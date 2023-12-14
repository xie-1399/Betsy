package Eyeriss.RLC

import spinal.core._
import spinal.lib._
import Eyeriss._
import spinal.lib.fsm._

/*
  the decode of weight will pass to the global buffer
  the decode state machine will deal the encode format and always drive the data out
*/


class Decode(config:EyerissParameters) extends Component {

  val io = new Bundle{
    val dataIn = slave (Stream (Bits(config.RLCDataWidth bits)))
    val dataOut = master (Stream (Bits(16 bits)))
    val end = out Bool()
    val error = out Bool()
  }
  noIoPrefix()

  def runSize = 5
  def levelSize = 16

  val busy = RegInit(False).setWhen(io.dataIn.fire)
  val end = RegInit(True).clearWhen(io.dataIn.fire)
  val error = RegInit(False)
  io.dataIn.ready := !busy
  io.end := end
  io.dataOut.valid := False
  io.dataOut.payload := 0
  io.error := error

  val codes = RegNextWhen(io.dataIn.payload,busy)
  val levels = Vec(Bits(levelSize bits),3)
  val runs = Vec(Bits(runSize bits),3)

  val slices = codes(62 downto 0).subdivideIn(runSize + levelSize bits)
  val term = codes.msb
  (0 until 3).map(idx => {
    runs(idx) := slices(idx)(4 downto 0)
    levels(idx) := slices(idx)(20 downto 5)
  })

  val decodeFsm = new StateMachine{
    val Idle = new State with EntryPoint
    val Run = new State
    val Level = new State
    val Finish = new State

    val iter = Counter(0,3)
    val runCounter = Counter(0,32)
    Idle.whenIsActive{
      when(busy){
        goto(Run)
      }
    }
    Run.whenIsActive{
      when(term){
        error := True
      }
      io.dataOut.valid := True
      io.dataOut.payload := 0
      when(io.dataOut.fire){
        runCounter.increment()
        when(runCounter.value === runs(iter).asUInt){
          goto(Level)
        }
      }
    }
    Level.whenIsActive{
      io.dataOut.valid := True
      io.dataOut.payload := levels(iter)
      when(io.dataOut.fire){
        iter.increment()
        when(iter.willOverflow){
          goto(Finish)
        }.otherwise(goto(Run))
      }
    }
    Finish.whenIsActive{
      busy := False
      end := True
      goto(Idle)
    }
  }

}

object Decode extends App{
  val config = EyerissParameters()
  val withString = true
  if(!withString){
    SpinalConfig(enumPrefixEnable = false).withoutEnumString().generateVerilog(new Decode(config))
  }
  else{
    SpinalVerilog(new Decode(config))
  }

}
