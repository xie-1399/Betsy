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

object SamplerInstruction extends SpinalEnum{
  val nop,load,matmul,datamove,simd,configure = newElement()
}

case class SamplerFlags() extends Bundle{
  val valid = Bool()
  val ready = Bool()
  val instruction = SamplerInstruction()
}

case class Sample(pcWidth:Int) extends Bundle{
  val flags = SamplerFlags()
  val pc = UInt(pcWidth bits)

  def initAssign() = {
    this.flags.assignFromBits(B(0,16 bits))
    this.pc := 0
  }
}


class Sampler(arch:Architecture) extends BetsyModule{

  val io = new Bundle{
    val flags = in (SamplerFlags())
    val pc = in UInt(arch.pcWidth bits)
    val interval = in UInt(arch.pcWidth / 2 bits)
    val sample = master Stream Sample(arch.pcWidth)
  }

  val finish = io.flags.valid && io.flags.ready
  val cycles = Reg(UInt(arch.pcWidth/2 bits)).init(0) /* instruction running cycles */
  when(io.flags.valid && io.flags.ready){
    cycles.clearAll()
  }.elsewhen(io.flags.valid){
    cycles := cycles + 1
  }
  val sample = Sample(arch.pcWidth)
  val cycleCounter = Reg(UInt(arch.pcWidth/2 bits)).init(0)  /* the counter is used to keep the sampler interval */

  when(io.interval =/= 0 && io.flags.valid) {
    sample.pc := io.pc
    sample.flags := io.flags
    when(cycleCounter === 0) {
      cycleCounter := io.interval - 1
      io.sample.valid := True
    }.otherwise {
      cycleCounter := cycleCounter - 1
      io.sample.valid := False
    }
  }.otherwise {
    sample.initAssign()
    io.sample.valid := False
    cycleCounter.clearAll()
  }
  io.sample.payload.pc := sample.pc
  io.sample.payload.flags := sample.flags
}

object Sampler extends App{
  val arch = Architecture()
  SpinalSystemVerilog(new Sampler(arch))
}