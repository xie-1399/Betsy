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


class Accumulator[T<:Data with Num[T]](gen:HardType[T],SimdWidth:Int,depth:Int) extends BetsyModule{

  val io = new Bundle{
    val dataIn = slave(Stream(Vec(gen(),SimdWidth)))
    val dataOut = master(Stream(Vec(gen(),SimdWidth)))
    val control = slave(Stream(AccumulatorControl(depth)))
  }

  val accumulator = new DualPortMem(gen,depth)

  /* two ports accumulator port A write and port B read*/
  val portA = accumulator.io.portA
  val portB = accumulator.io.portB

  /* Port A */
  portA.blockPort()
  // io.dataOut <> portA.dataOut


  /* Port B */
  portB.blockPort()

//  when(io.control.valid && io.control.payload.write){
//    when(io.control.payload.accumulate){
//
//    }.otherwise{
//      /* just write into the accumulator */
//      portA
//    }
//  }

  io.control.ready := True
  io.dataOut <> io.dataIn
}

object Accumulator extends App{
  SpinalSystemVerilog(new Accumulator(UInt(4 bits),4,128))
}