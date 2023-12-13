package Eyeriss.RLC

import spinal.core._
import spinal.lib._
import Eyeriss._
import spinal.lib.fsm._

/* the decode of weight will pass to the global buffer */


class Decoder(config:EyerissParameters) extends Component {

  val io = new Bundle{
    val dataIn = slave (Stream (Bits(config.RLCDataWidth bits)))
    val dataOut = master (Stream (Bits(16 bits)))
    val end = out Bool()
  }

  val busy = RegInit(False)
  val end = RegInit(True)
  io.dataIn.ready := !busy
  when(io.dataIn.fire){
    busy := True
    end  := False
  }
  io.end := end
  io.dataOut.valid := False
  io.dataOut.payload := 0

  val codes = RegNextWhen(io.dataIn.payload,busy)
  val levels = Vec(Bits(16 bits),3)
  val runs = Vec(Bits(5 bits),3)

  //Todo mapping it to the RLC
  //runs.map()

  for(idx <- 0 until 6){
    var ptr = 63
    if(idx % 2 == 0){
      runs(idx / 2) := codes(ptr downto ptr - 4)
      ptr = ptr - 4
    }else{
      levels(idx / 2) := codes(ptr downto ptr - 15)
      ptr = ptr - 15
    }
  }

  val decodeFsm = new StateMachine{
    val Idle = new State with EntryPoint
    val Run = new State
    val Level = new State
    val Finish = new State

    val iter = Counter(0,3)
    Idle.whenIsActive{
      when(busy){
        goto(Run)
      }
    }

    Run.whenIsActive{
      io.dataOut.valid := True
      io.dataOut.payload := 0
      when(io.dataOut.fire){
        //Todo
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

object Decoder extends App{
  val config = EyerissParameters()
  SpinalVerilog(new Decoder(config))
}
