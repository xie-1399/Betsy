package Betsy

/**
 ** Betsy follow the MiT Licence.(c) xxl, All rights reserved **
 ** Update Time : 2024/4/21      SpinalHDL Version: 1.94       **
 ** You should have received a copy of the MIT License along with this library **
 ** the sampler can be used to out the pc status for a interval time and show the stages information **
 */

import Betsy.Until._
import spinal.core._
import spinal.lib._

case class DecoupledFlags() extends Bundle{
  val ready = Bool()
  val valid = Bool()

  def connect(data:Stream[Data]) = {
    ready := data.ready
    valid := data.valid
  }
}

case class SamplerFlags() extends Bundle{
  val instruction = new DecoupledFlags
  val memPortA = new DecoupledFlags
  val memPortB = new DecoupledFlags
  val dram0 = new DecoupledFlags
  val dram1 = new DecoupledFlags
  val dataflow = new DecoupledFlags
  val acc = new DecoupledFlags
  val array = new DecoupledFlags
}

case class Sample(pcWidth:Int) extends Bundle{
  val flags = SamplerFlags()
  val pc = UInt(pcWidth bits)

  def initAssign() = {
    this.flags.assignFromBits(B(0,16 bits))
    this.pc := 0
  }
}

object Sample{
  def apply(pcWidth:Int) = {
    val sample = new Sample(pcWidth)
    sample.pc := 0
    sample.flags.assignFromBits(B(0,16 bits))
    sample
  }
}

// Todo update it with My sampler
class Sampler(arch:Architecture,blockSize:Int) extends BetsyModule{

  val io = new Bundle{
    val flags = in(SamplerFlags())
    val pc = in UInt(arch.pcWidth bits)
    val interval = in UInt(arch.pcWidth/2 bits)
    val sample = master Stream withLast(new Sample(arch.pcWidth))
  }

  val cycleCounter = Reg(UInt(arch.pcWidth/2 bits)).init(0)  /* the counter is used to keep the sampler interval */

  val outSample = RegInit(Sample(arch.pcWidth))
  val outValid = RegInit(False)
  val outLast = RegInit(False)
  val outCounter = Reg(UInt(32 bits)).init(0)

  val sampleReady = Bool()
  val sample = new Sample(arch.pcWidth)

  io.sample.valid := outValid
  io.sample.payload.last := outLast
  io.sample.payload.payload := outSample

  when(io.interval =/= 0){
    sample.pc := io.pc
    sample.flags := io.flags
    when(cycleCounter === 0){
      sampleReady.set()
      cycleCounter := io.interval - 1
    }.otherwise{
      sampleReady.clear()
      cycleCounter := cycleCounter - 1
    }
  }.otherwise{
    sample.initAssign()
    sampleReady.clear()
    cycleCounter.clearAll()
  }

  when(!outValid || io.sample.ready){
    when(sampleReady){
      when(outCounter === blockSize){
        outLast.set()
        outCounter.clearAll()
      }.otherwise{
        outLast.clear()
        outCounter := outCounter + 1
      }

      outValid := True
      outSample := sample
    }.otherwise{
      outValid.clear()
      outLast.clear()
    }
  }

}


object Sampler extends App{
  val arch = Architecture()

  SpinalSystemVerilog(new Sampler(arch,16))

}