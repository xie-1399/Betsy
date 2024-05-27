package Betsy


/**
 ** Betsy follow the MiT Licence.(c) xxl, All rights reserved **
 ** Update Time : 2024/5/27      SpinalHDL Version: 1.94       **
 ** You should have received a copy of the MIT License along with this library **
 ** Todo tested ...**
 */

import BetsyLibs.{BetsyCounter, CountBy}
import spinal.core._
import spinal.lib._
import Until._

class StrideHandler[T <: Bundle with Address with Size with Stride with Reverse,
  S <: Bundle with Address with Size](inGen:HardType[T],outGen:HardType[S],depth:Long,StrideDepth:Int) extends BetsyModule {

  val io = new Bundle{
    val into = slave Stream inGen()
    val output = master Stream outGen()
  }

  val stride = U(1) << io.into.payload.stride
  val sizeCounter = new BetsyCounter(depth) //using the Counter can also work out
  sizeCounter.io.value.ready := False
  sizeCounter.io.resetValue := False

  val countBy = new CountBy(depth)
  countBy.io.value.ready := False
  countBy.io.step := stride
  countBy.io.resetValue := False

  /* the other elem and size restrict */
  for((elem,idx) <- io.into.payload.elements.zipWithIndex){
    if(io.output.payload.elements(idx)._1 == elem._1){
      io.output.payload.elements(idx)._2 := elem._2
    }
    if(io.output.payload.elements(idx)._1 == "size"){
      io.output.payload.elements(idx)._2 := U(0)
    }
  }

  when(sizeCounter.io.value.payload === io.into.size){
    io.into.ready.set()
    sizeCounter.io.resetValue := io.output.fire
    countBy.io.resetValue := io.output.fire
  }.otherwise{
    io.into.ready := False
    sizeCounter.io.value.ready := io.output.fire
    countBy.io.value.ready := io.output.fire
  }

  io.output.payload.address := Mux(io.into.reverse,io.into.address - countBy.io.value.payload,io.into.address + countBy.io.value.payload)
  io.output.valid := io.into.valid
}
