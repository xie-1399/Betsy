package Eyeriss.PE
import spinal.core._
import spinal.lib._
import Eyeriss._
import spinal.lib.sim.{StreamDriver, StreamMonitor, StreamReadyRandomizer}

case class ReluConfig(ReluVecNum:Int = 1)

class Relu(p:EyerissParameters,c:ReluConfig) extends Component {

  require(p.DataType == SInt16)

  val io = new Bundle{
    val dataIn = slave Stream(Vec(SInt(p.DataWidth bits),c.ReluVecNum))
    val act = master Stream(Vec(Bits(p.DataWidth bits),c.ReluVecNum))
  }
  /* no pipeline and only trans use the relu unit */
  // io.act.translateFrom(io.dataIn)((to,from) => to := Mux(from > 0,from.asBits,B(0,p.DataWidth bits)))
  noIoPrefix()
  io.act.arbitrationFrom(io.dataIn)

  (0 until c.ReluVecNum).map{
    idx =>
      io.act.payload(idx) := Mux(io.dataIn.payload(idx) > 0,io.dataIn.payload(idx).asBits,B(0,p.DataWidth bits))
  }
}

object GenRtl extends App{
  SpinalVerilog(new Relu(EyerissParameters(),ReluConfig(32)))
}

object Relu extends App{
  import spinal.core.sim._

  SpinalSimConfig().workspacePath("simulation").withFstWave.compile{
    val dut = new Relu(EyerissParameters(),ReluConfig())
    dut
  }.doSimUntilVoid{
    dut =>
      dut.clockDomain.forkStimulus(10)
      StreamReadyRandomizer(dut.io.act,dut.clockDomain)
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
            if(dut.io.dataIn.payload(0).toBigInt > 0){
              assert(dut.io.act.payload(0).toBigInt == payload(0).toBigInt)
            }else{
              assert(dut.io.act.payload(0).toBigInt == 0)
            }
        }
        dut.clockDomain.waitRisingEdgeWhere(matches == 100)
      }

      thread.join()
      simSuccess()
  }

}