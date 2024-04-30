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
import BetsyLibs._

class SystolicArray[T <: Data with Num[T]](gen:HardType[T],height:Int,width:Int) extends BetsyModule{
  val io = new Bundle{
    val control = slave Stream(SystolicArrayControl())
    val weight = slave Stream(Vec(gen, height))
    val input = slave Stream(Vec(gen, width))
    val output = master Stream(Vec(gen, height))
  }

  val array = new InnerSystolicArray(gen,height,width)
  val arrayPropagationDelay = height + width - 1 /* load finish and get the first row result dealy */
  val outQueue = new BetsyFIFO(cloneOf(array.io.output),arrayPropagationDelay)

  /* control signals */
  val loadWeight = io.control.valid && io.control.load && !io.control.zeroes
  val loadZeroes = io.control.valid && io.control.load && io.control.zeroes
  val runInput = io.control.valid && !io.control.load && !io.control.zeroes
  val runZeroes = io.control.valid && !io.control.load && io.control.zeroes
  val running = ((runInput && io.input.fire) || runZeroes) && io.output.ready /* out is ready */
  val loading = loadZeroes || (io.weight.fire && loadWeight)

  /* a counter shows input is Done */
  val arrayCounter = Reg(UInt(log2Up(arrayPropagationDelay) bits)).init(0)
  when(running){
    arrayCounter := arrayPropagationDelay
  }.otherwise{
    when(arrayCounter > 0){
     arrayCounter := arrayCounter - 1
    }
  }
  val inputDone = arrayCounter === 0
  when(io.control.zeroes){
    array.io.input.foreach(_ := zero(gen()))
    array.io.weight.foreach(_ := zero(gen()))
  }.otherwise{
    array.io.input := io.input.payload
    array.io.weight := io.weight.payload
  }

  array.io.load := loading && inputDone
  io.input.ready := io.output.ready && runInput
  io.weight.ready := loadWeight && inputDone
  io.control.ready := (!io.control.load && (io.control.zeroes || io.input.valid) && io.output.ready) ||
    (io.control.load && (io.control.zeroes || io.weight.valid) && inputDone)

  /* connect the output */
  outQueue.io.push.valid := Delay(running,arrayPropagationDelay,init = False)
  outQueue.io.push.payload := array.io.output
  io.output <> outQueue.io.pop
}


/* using a stateMachine also can work -> V2 works*/
//class SystolicArrayV2[T <: Data with Num[T]](gen:HardType[T],height:Int,width:Int,performance:Boolean = false) extends BetsyModule{
//
//  val io = new Bundle {
//    val control = slave Stream (SystolicArrayControl())
//    val weight = slave Stream (Vec(gen, height))
//    val input = slave Stream (Vec(gen, width))
//    val output = master Stream (Vec(gen, height))
//  }
//
//  val array = new InnerSystolicArray(gen,height,width)
//  io.control.ready := False
//  io.weight.ready := False
//  io.input.ready := io.output.ready
//  val runZero = False
//  val inputCounter = Counter(height + width - 1)
//  val inputDone = inputCounter === height + width - 1
//  val outQueue = StreamFifo(cloneOf(io.output.payload),height + width - 1)
//
//  val arrayFSM = new StateMachine{
//    val Idle = new State with EntryPoint
//    val Load = new State
//    val Run = new State
//
//    Idle.whenIsActive{
//      inputCounter := 0
//      when(io.control.load && io.control.valid){
//        io.control.ready := True
//        goto(Load)
//      }
//      when(io.control.fire && !io.control.load && (io.input.fire || io.control.zeroes) ){
//        inputCounter.increment()
//        goto(Run)
//      }
//    }
//
//    Load.whenIsActive{
//      io.control.ready := True //last io.load weight is the bias
//      when(io.control.fire && io.control.load && inputDone){
//        io.weight.ready := True
//      }
//      when(io.input.fire && io.control.fire && !io.control.load){
//        inputCounter := 1
//        goto(Run)
//      }
//    }
//
//    Run.whenIsActive{
//      /* fill the input and zeroes */
//      io.control.ready := io.output.ready
//      when(io.control.fire && !io.control.load && (io.input.fire || io.control.zeroes)){
//        runZero.setWhen(io.control.zeroes)
//        inputCounter.increment() //run zeroes or input
//      }
//      when(inputCounter.willOverflow){
//        when(io.control.fire && !io.control.load && (io.input.fire || io.control.zeroes)){
//          inputCounter
//        }
//      }
//    }
//  }
//  array.io.load := io.control.load && io.control.fire
//  when(io.control.zeroes && io.control.fire) {
//    array.io.input.foreach(_ := zero(gen()))
//    array.io.weight.foreach(_ := zero(gen()))
//  }.otherwise {
//    array.io.input := io.input.payload
//    array.io.weight := io.weight.payload
//  }
//  io.output <> outQueue.io.pop
//
//  /* performance counter */
//  val performanceCount = performance.generate{
//    new Area {
//      val loadCycles = Reg(UInt(64 bits)).init(0)
//      val macCycles = Reg(UInt(64 bits)).init(0)
//      arrayFSM.Load.whenIsActive{
//        loadCycles := loadCycles + 1
//      }
//      arrayFSM.Run.whenIsActive{
//        macCycles := macCycles + 1
//      }
//      /* printf the debug info*/
//      when(io.input.valid && io.input.ready) {
//        printf(s"SystolicArray: received input ${io.input.payload}\n")
//      }
//      when(io.weight.valid && io.weight.ready) {
//        printf(s"SystolicArray: loaded weights ${io.weight.payload}\n")
//      }
//    }
//  }
//  /* assert running error */
//  assert(io.control.fire && io.control.load && io.input.fire,"error while load weight and load input at the same time !!!")
//}


object SystolicArrayV2 extends App{
  SpinalSystemVerilog(new SystolicArray(SInt(8 bits),4,4))
}