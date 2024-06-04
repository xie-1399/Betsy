package Betsy

import Betsy.Until._
import spinal.core._
import spinal.lib._
import BetsyLibs._

/**
 ** Betsy follow the MiT Licence.(c) xxl, All rights reserved **
 ** Update Time : 2024/5/27      SpinalHDL Version: 1.94       **
 ** You should have received a copy of the MIT License along with this library **
 ** **
 */

// looks like will work in my build way

class Accumulator[T<:Data with Num[T]](gen:HardType[T],SimdHeight:Int,depth:Int) extends BetsyModule{

  val io = new Bundle{
    val dataIn = slave(Stream(Vec(gen(),SimdHeight)))
    val dataOut = master(Stream(Vec(gen(),SimdHeight)))
    val control = slave(Stream(AccumulatorControl(depth)))
  }

  val accumulator = new DualPortMem(Vec(gen,SimdHeight),depth)  // Todo with the bits width...
  /* two ports accumulator port A write and port B read*/
  val portA = accumulator.io.portA
  val portB = accumulator.io.portB

  /* Port A (read and write) */
  portA.blockPort()
  /* Port B （just read）*/
  portB.blockPort()
  // val accMultiQueue = new MultiEnqControl(2) /* for the accumulate control (read + accumulate + write into) */

  val inputDemux = new BetsyStreamDemux(cloneOf(io.dataIn.payload),2)
  inputDemux.io.InStream <> io.dataIn
  inputDemux.io.sel.valid := io.control.valid
  inputDemux.io.sel.payload := io.control.payload.accumulate.asUInt

  val vecAdder = new VecAdder(gen,size = SimdHeight)
  vecAdder.io.left <> inputDemux.io.OutStreams(1).queueLowLatency(size = 2,latency = 1) /* using a delay fifo */
  vecAdder.io.right <> portB.dataOut

  val inputMux = new BetsyStreamMux(cloneOf(io.dataIn.payload),2)
  inputMux.io.InStreams(0) <> inputDemux.io.OutStreams(0)
  inputMux.io.InStreams(1) <> vecAdder.io.output
  inputMux.io.sel.valid := io.control.valid
  inputMux.io.sel.payload := io.control.payload.accumulate.asUInt
  inputMux.io.OutStream.ready := portA.dataIn.ready

  when(io.control.payload.write){
    when(io.control.payload.accumulate){ // Todo with the accumulate logic
      /* write to the port A */
      portA.control.valid := io.control.valid
      portA.control.payload.write := io.control.payload.write
      portA.control.payload.address := io.control.payload.address
      /* read from the port B*/
      portB.control.valid := io.control.valid
      portB.control.payload.address := io.control.payload.address
      portB.control.payload.write := False

      io.control.ready := portA.dataIn.fire
    }.otherwise{
      /* just write into the accumulator from PortA */
      portA.control.payload.address := io.control.payload.address
      portA.control.payload.write := io.control.payload.write
      portA.control.valid := io.control.valid
      io.control.ready := portA.dataIn.fire
    }
  }.otherwise{
    /* just read from the port A */
    portA.control.valid := io.control.valid
    portA.control.payload.address := io.control.payload.address
    portA.control.payload.write := io.control.payload.write
    io.control.ready := portA.dataOut.fire
  }
  io.dataOut <> portA.dataOut
}

object Accumulator extends App{
  SpinalSystemVerilog(new Accumulator(UInt(4 bits),4,128))
}