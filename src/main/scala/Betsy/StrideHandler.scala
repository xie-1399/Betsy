package Betsy


/**
 ** Betsy follow the MiT Licence.(c) xxl, All rights reserved **
 ** Update Time : 2024/5/27      SpinalHDL Version: 1.94       **
 ** You should have received a copy of the MIT License along with this library **
 ** the size is show the address stride length
 ** when the size is 4 , step is 2 -> stride 2 with 4 times
 ** Test Status : PASS :)         Version:0.2  **
 */

import BetsyLibs.{BetsyCounter, CountBy}
import spinal.core._
import spinal.lib._
import Until._

/* the v0.2 clean and extend the error signal about illegal address and illegal stride format */

class StrideHandler[T <: Bundle with Address with Size with Stride with Reverse,
  S <: Bundle with Address with Size](inGen:HardType[T],outGen:HardType[S],depth:Long) extends BetsyModule {

  val io = new Bundle{
    val into = slave Stream inGen()
    val output = master Stream outGen()
    val error = out Bool()
  }

  def block(): Unit = {
    io.into.valid.clear()
    io.into.payload.clearAll()
    io.output.ready.clear()
  }

  val stride = U(1) << io.into.payload.stride

  val sizeCounter = new BetsyCounter(depth)
  sizeCounter.io.value.ready := False
  sizeCounter.io.resetValue := False

  val countBy = new CountBy(depth)
  countBy.io.value.ready := False
  countBy.io.step := stride.resized
  countBy.io.resetValue := False

  when(io.into.size === 0){ // when the size is zero , translate one cycle
    for (outelem <- io.output.payload.elements) {
      for (inelem <- io.into.payload.elements) {
        if (outelem._1 == inelem._1 && outelem._1 != "address" && outelem._1 != "size") {
          outelem._2 := inelem._2
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
      io.into.ready := io.output.ready
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

  val error = RegInit(False)
  when(io.into.valid){
    error := (io.into.size === 0 && stride =/= 1) || io.output.payload.address >= depth
    assert(!(io.into.size === 0 && stride =/= 1),"the size is 0 but stride is over 0 in the [StrideHandler]!!!")
    assert(io.output.payload.address < depth,"the address is illegal in the [StrideHandler]!!!" )
  }
  io.error := error
}


