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


class Accumulator[T<:Data with Num[T]](gen:HardType[T],SimdHeight:Int,depth:Int) extends BetsyModule{

  val io = new Bundle{
    val dataIn = slave(Stream(Vec(gen(),SimdHeight)))
    val dataOut = master(Stream(Vec(gen(),SimdHeight)))
    val control = slave(Stream(AccumulatorControl(depth)))
  }

  val accumulator = new DualPortMem(gen,depth)
  /* two ports accumulator port A write and port B read*/
  val portA = accumulator.io.portA
  val portB = accumulator.io.portB

  /* Port A */
  portA.blockPort()
  /* Port B */
  portB.blockPort()

  val vecAdder = new VecAdder(gen,size = SimdHeight)
  //vecAdder.io.left <> portA.dataOut
  //vecAdder.io.right <> portB.dataOut

  when(io.control.valid && io.control.payload.write){
    when(io.control.payload.accumulate){
      /* write to the port A */

    }.otherwise{
      /* just write into the accumulator */
      portA
    }
  }

  io.control.ready := True
  io.dataOut <> io.dataIn
}

object Accumulator extends App{
  SpinalSystemVerilog(new Accumulator(UInt(4 bits),4,128))
}