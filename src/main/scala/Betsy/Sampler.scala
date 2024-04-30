package Betsy

/**
 ** Betsy follow the MiT Licence.(c) xxl, All rights reserved **
 ** Update Time : 2024/4/21      SpinalHDL Version: 1.94       **
 ** You should have received a copy of the MIT License along with this library **
 ** Todo add the program counter for the decoder **
 */

import Betsy.Until.BetsyModule
import spinal.core._
import spinal.lib._


case class SamplerFlags() extends Bundle{

  // val instruction = new

}

class Sampler() extends BetsyModule{

  val io = new Bundle{

    val programCounter = in UInt(32 bits)

  }

  val cycleCounter = Reg(UInt(16 bits)).init(0)  /* the cycles goes running */




}
