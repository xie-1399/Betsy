package Betsy

import Betsy.Until._
import spinal.core._
import spinal.lib._
import BetsyLibs._
/**
 ** Betsy follow the MiT Licence.(c) xxl, All rights reserved **
 ** Update Time : 2024/5/27      SpinalHDL Version: 1.94       **
 ** You should have received a copy of the MIT License along with this library **
 ** Test Status : PASS :)         Version:0.1 **
 */

class Accumulator[T<:Data with Num[T]](gen:HardType[T],SimdHeight:Int,depth:Long) extends BetsyModule{

  val io = new Bundle{
    val dataIn = slave(Stream(Vec(gen(),SimdHeight)))
    val dataOut = master(Stream(Vec(gen(),SimdHeight)))
    val control = slave(Stream(new AccumulatorControl(depth)))
  }

  val accumulator = new DualPortMem(Vec(gen,SimdHeight),depth)
  /* two ports accumulator port A write and port B read*/
  val portA = accumulator.io.portA
  val portB = accumulator.io.portB

  /* Port A (read and write) */
  portA.blockPort()
  /* Port B （just read）*/
  portB.blockPort()
  val accMultiQueue = MultiEnqControl(2) /* for the accumulate control (read + accumulate + write into) */

  val inputDemux = new BetsyStreamDemux(cloneOf(io.dataIn.payload),2)
  inputDemux.io.InStream <> io.dataIn
  inputDemux.io.sel.valid := io.control.valid
  inputDemux.io.sel.payload := io.control.payload.accumulate.asUInt

  val vecAdder = new VecAdder(gen,size = SimdHeight)
  val fifo = new BetsyFIFO(cloneOf(inputDemux.io.OutStreams(1).payload),1)
  fifo.io.push.valid := inputDemux.io.OutStreams(1).valid && !fifo.io.pop.valid
  fifo.io.push.payload := inputDemux.io.OutStreams(1).payload
  inputDemux.io.OutStreams(1).ready := fifo.io.push.ready
  vecAdder.io.left << fifo.io.pop
  vecAdder.io.right << portB.dataOut

  val inputMux = new BetsyStreamMux(cloneOf(io.dataIn.payload),2)
  inputMux.io.InStreams(0) <> inputDemux.io.OutStreams(0)
  inputMux.io.InStreams(1) <> vecAdder.io.output
  inputMux.io.sel.valid := io.control.valid
  inputMux.io.sel.payload := io.control.payload.accumulate.asUInt
  inputMux.io.OutStream.ready := portA.dataIn.ready

  when(io.control.payload.write){
    when(io.control.payload.accumulate){
      /* write to the port A */
      portA.control.valid := vecAdder.io.output.valid
      portA.control.payload.write := io.control.payload.write
      portA.control.payload.address := io.control.payload.address
      portA.dataIn << inputMux.io.OutStream
      /* read from the port B*/
      portB.control.valid := io.control.valid
      portB.control.payload.address := io.control.payload.address
      portB.control.payload.write := False
      io.control.ready := accMultiQueue.Readyenqueue2(io.control.payload.accumulate,portA.control.ready,portB.control.ready)
    }.otherwise{
      /* just write into the accumulator from PortA */
      portA.control.payload.address := io.control.payload.address
      portA.control.payload.write := io.control.payload.write
      portA.control.valid := io.control.valid
      portA.dataIn << inputMux.io.OutStream
      io.control.ready := portA.control.ready
    }
  }.otherwise{
    /* just read from the port A */
    portA.control.valid := io.control.valid
    portA.control.payload.address := io.control.payload.address
    portA.control.payload.write := io.control.payload.write
    io.control.ready := portA.control.ready // only read fot the controller
  }
  io.dataOut <> portA.dataOut
}