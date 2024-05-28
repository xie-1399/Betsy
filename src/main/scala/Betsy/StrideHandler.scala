package Betsy


/**
 ** Betsy follow the MiT Licence.(c) xxl, All rights reserved **
 ** Update Time : 2024/5/27      SpinalHDL Version: 1.94       **
 ** You should have received a copy of the MIT License along with this library **
 ** the size is show the address stride length
 ** when the size is 4 , step is 2 -> stride 2 with 4 times
 ** Todo test... **
 */

import BetsyLibs.{BetsyCounter, CountBy}
import spinal.core._
import spinal.lib._
import Until._

class StrideHandler[T <: Bundle with Address with Size with Stride with Reverse,
  S <: Bundle with Address with Size](inGen:HardType[T],outGen:HardType[S],depth:Long) extends BetsyModule {

  val io = new Bundle{
    val into = slave Stream inGen()
    val output = master Stream outGen()
  }

  val stride = U(1) << io.into.payload.stride //Todo with size * stride > depth
  val sizeCounter = new BetsyCounter(depth) //using the Counter can also work out
  sizeCounter.io.value.ready := False
  sizeCounter.io.resetValue := False

  val countBy = new CountBy(depth)
  countBy.io.value.ready := False
  countBy.io.step := stride.resized
  countBy.io.resetValue := False

  when(io.into.size === 0){
    for (outelem <- io.output.payload.elements) {
      for (inelem <- io.into.payload.elements) {
        if (outelem._1 == inelem._1 && outelem._1 != "address" && outelem._1 != "size") {
          outelem._2 := inelem._2 // connect other bundles
        }
      }
    }
    io.output.size := io.into.size
    io.output.address := io.into.address
    io.output.arbitrationFrom(io.into)
  }.otherwise {
    /* the other elem and size restrict */
    for (outelem <- io.output.payload.elements) {
      for (inelem <- io.into.payload.elements) {
        if (outelem._1 == "size" && inelem._1 == "size") {
          outelem._2 := U(0)
        }
        else if (outelem._1 == inelem._1 && outelem._1 != "address") {
          outelem._2 := inelem._2
        }
      }
    }
    when(sizeCounter.io.value.payload === io.into.size) {
      io.into.ready.set()
      sizeCounter.io.resetValue := io.output.fire
      countBy.io.resetValue := io.output.fire
    }.otherwise {
      io.into.ready := False
      sizeCounter.io.value.ready := io.output.fire
      countBy.io.value.ready := io.output.fire
    }

    io.output.payload.address := Mux(io.into.reverse, io.into.address - countBy.io.value.payload, io.into.address + countBy.io.value.payload)
    io.output.valid := io.into.valid
  }
  assert(!(io.into.size === 0 && stride > 0),"the size is 0 but stride is over 0 !!!")
}


