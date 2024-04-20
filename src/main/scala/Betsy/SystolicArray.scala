package Betsy

/**
 ** Betsy follow the MiT Licence.(c) xxl, All rights reserved **
 ** Update Time : 2024/4/15      SpinalHDL Version: 1.94     **
 ** You should have received a copy of the MIT License along with this library **
 ** Wrapper the InnerSystolicArray as a SystolicArray with control
 ** **
 */

import Betsy.Until._
import spinal.core._
import spinal.lib._
import spinal.lib.fsm._


class SystolicArray[T <: Data with Num[T]](gen:HardType[T],height:Int,width:Int,WhiteBox:Boolean = false) extends BetsyModule{
  /* using a stateMachine also can work -> V2 works*/
  val io = new Bundle{
    val control = slave Stream(SystolicArrayControl())
    val weight = slave Stream(Vec(gen, height))
    val input = slave Stream(Vec(gen, width))
    val output = master Stream(Vec(gen, height))
  }

  val array = new InnerSystolicArray(gen,height,width)
  val arrayPropagationDelay = height + width - 1 /* load finish and get the first row result dealy */

  val outQueue = StreamFifo(cloneOf(array.io.output),arrayPropagationDelay)

  /* control signals */
  val loadWeight = io.control.valid && io.control.load && !io.control.zeroes
  val loadZeroes = io.control.valid && !io.control.load && io.control.zeroes
  val runInput = io.control.valid && !io.control.load && !io.control.zeroes
  val runZeroes = io.control.valid && !io.control.load && io.control.zeroes

  val running = (runInput && io.input.fire || runZeroes) && io.output.ready /* out is ready */

  /* a counter shows input is Done */
  val arrayCounter = Counter(arrayPropagationDelay)
  when(running){
    arrayCounter.increment()
  }.otherwise{

  }

  when(io.control.zeroes){
    array.io.input := zero(array.io.input)
    array.io.weight := zero(array.io.weight)
  }.otherwise{
    array.io.input := io.input.payload
    array.io.weight := io.weight.payload
  }
  io.input.ready := io.output.ready && runInput
  // io.weight.ready := loadWeight &&

  /* connect the output */
  outQueue.io.push.valid := Delay(running,arrayPropagationDelay)
  io.output <> outQueue.io.pop
}


class SystolicArrayV2[T <: Data with Num[T]](gen:HardType[T],height:Int,width:Int,performance:Boolean = false) extends BetsyModule{

  val io = new Bundle {
    val control = slave Stream (SystolicArrayControl())
    val weight = slave Stream (Vec(gen, height))
    val input = slave Stream (Vec(gen, width))
    val output = master Stream (Vec(gen, height))
  }

  val array = new InnerSystolicArray(gen,height,width)
  io.control.ready := False
  io.weight.ready := False
  io.input.ready := io.output.ready
  val inputCounter = Counter(height + width - 1)
  val outQueue = StreamFifo(io.output.payload.clone(),height + width - 1)

  val arrayFSM = new StateMachine{
    val Idle = new State with EntryPoint
    val Load = new State
    val Input = new State
    val Mac = new State

    Idle.whenIsActive{
      when(io.control.load && io.control.valid){
        io.control.ready := True
        goto(Load)
      }
      when(io.input.fire){
        inputCounter.increment()
        goto(Input)
      }
    }

    Load.whenIsActive{
      io.control.ready := True
      when(io.input.fire){
        inputCounter.increment()
        goto(Input)
      }
    }

    Input.whenIsActive{
      /* fill the input and zeroes */
      when(io.input.fire){
        inputCounter.increment()
      }
      when(inputCounter.willOverflow){
        goto(Mac)
      }
    }

    Mac.whenIsActive{

    }

  }

  array.io.load := io.control.load && io.control.fire
  when(io.control.zeroes && io.control.fire) {
    array.io.input := zero(array.io.input)
    array.io.weight := zero(array.io.weight)
  }.otherwise {
    array.io.input := io.input.payload
    array.io.weight := io.weight.payload
  }
  // io.output <> outQueue.io.pop

  /* performance counter */
  val performanceCount = performance.generate{
    new Area {
      val loadCycles = Reg(UInt(64 bits)).init(0)
      val macCycles = Reg(UInt(64 bits)).init(0)
    }
  }
}


object SystolicArrayV2 extends App{
  SpinalSystemVerilog(new SystolicArrayV2(SInt(8 bits),4,4))
}