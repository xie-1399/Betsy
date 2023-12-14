package Eyeriss.PE
import spinal.core._
import spinal.lib._
import Eyeriss._
import spinal.lib.sim.{StreamDriver, StreamMonitor}

class Relu(p:EyerissParameters) extends Component {

  require(p.DataType == SInt16)

  val io = new Bundle{
    val dataIn = slave Stream(SInt(p.DataWidth bits))
    val act = master Stream(Bits(p.DataWidth bits))
  }
  /* no pipeline and only trans use the relu unit */
  // io.act.translateFrom(io.dataIn)((to,from) => to := Mux(from > 0,from.asBits,B(0,p.DataWidth bits)))

  io.act.arbitrationFrom(io.dataIn)
  io.act.payload := Mux(io.dataIn.payload > 0,io.dataIn.payload.asBits,B(0,p.DataWidth bits))
}

object Relu extends App{
  import spinal.core.sim._

  SpinalSimConfig().workspacePath("simulation").compile{
    val dut = new Relu(EyerissParameters())
    dut
  }.doSimUntilVoid{
    dut =>
      dut.clockDomain.forkStimulus(10)

      val thread = fork{
        var matches = 0
        StreamDriver(dut.io.dataIn,dut.clockDomain){
          drive =>
            dut.io.dataIn.randomize()
            true
        }

        StreamMonitor(dut.io.dataIn,dut.clockDomain){
          payload =>
            matches += 1
            if(dut.io.dataIn.payload.toBigInt > 0){
              assert(dut.io.act.payload.toBigInt == payload.toBigInt)
            }else{
              assert(dut.io.act.payload.toBigInt == 0)
            }
        }
        dut.clockDomain.waitRisingEdgeWhere(matches == 100)
      }

      thread.join()
      simSuccess()
  }


}