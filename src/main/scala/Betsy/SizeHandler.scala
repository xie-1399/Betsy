package Betsy

import Betsy.Until._
import BetsyLibs._
import spinal.core._
import spinal.lib._

/**
 ** Betsy follow the MiT Licence.(c) xxl, All rights reserved **
 ** Update Time : 2024/5/29      SpinalHDL Version: 1.94       **
 ** You should have received a copy of the MIT License along with this library **
 ** only control the size (only when the size is on -> output it)
 ** the test also contain in the Stride Handler
 ** Test Status : PASS :)         Version:0.1 **
 */

/* Todo can simplify it and common with the Stride handler */

class SizeHandler[T <: Bundle with Size, S <: Bundle](inGen: HardType[T], outGen: HardType[S], depth: Long) extends BetsyModule{

  val io = new Bundle{
    val into = slave Stream inGen()
    val output = master Stream outGen()
  }
  val sizeCounter = BetsyCounter(depth)
  for (outelem <- io.output.payload.elements) {
    for (inelem <- io.into.payload.elements) {
      if (outelem._1 == inelem._1){
        outelem._2 := inelem._2 // connect same name bundles
      }
    }
  }

  when(sizeCounter.io.value.payload === io.into.size){
    io.into.ready := io.output.ready
    sizeCounter.io.resetValue := io.output.fire
  }.otherwise{
    io.into.ready := False
    sizeCounter.io.value.ready := io.output.fire
  }

  io.output.valid := io.into.valid
}

class LoadSizeHandler[T <: Bundle with Size, S <: Bundle](inGen: HardType[T], outGen: HardType[S], depth: Long) extends BetsyModule{

  val io = new Bundle{
    val into = slave Stream inGen()
    val output = master Stream outGen()
  }
  val sizeCounter = BetsyCounter(depth)
  for (outelem <- io.output.payload.elements) {
    for (inelem <- io.into.payload.elements) {
      if (outelem._1 == inelem._1){
        outelem._2 := inelem._2 // connect same name bundles
      }
    }
  }

  when(sizeCounter.io.value.payload === io.into.size - 1){
    io.into.ready := io.output.ready
    sizeCounter.io.resetValue := io.output.fire
  }.otherwise{
    io.into.ready := False
    sizeCounter.io.value.ready := io.output.fire
  }

  io.output.valid := io.into.valid
}



object SizeHandlerDemo extends App{
  // test for the generate...
  val rtl = SpinalSystemVerilog(new SizeHandler(new LocalDataFlowControlWithSize(1024),
    new DataFlowSel(LocalDataFlowControl.locaDataFlowNums),depth = 1024))
}