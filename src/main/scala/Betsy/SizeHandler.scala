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

/* just think about the length of the translation */
/* when the size valid is always true , seems will translate with (size + 1) cycles */
/* the size width is under log2up(depth) and so no check error */

class SizeHandler[T <: Bundle with Size, S <: Bundle](inGen: HardType[T], outGen: HardType[S], depth: Long) extends BetsyModule{

  val io = new Bundle{
    val into = slave Stream inGen()
    val output = master Stream outGen()
  }

  def block(): Unit = {
    io.into.valid.clear()
    io.into.payload.clearAll()
    io.output.ready.clear()
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