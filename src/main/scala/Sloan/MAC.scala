package Sloan

/**
 ** Sloan follow the MiT Licence.(c) xxl, All rights reserved **
 ** Update Time : 2024/4/14      SpinalHDL Version: 1.94       **
 ** You should have received a copy of the MIT License along with this library **
 ** MAC Unit in the Accelerator
 ** Todo tobe tested !!! **
 */

import BSUntils.SIMCFG
import Sloan.Until._
import spinal.core._
import spinal.lib._

import scala.util.Random

class MAC[T<: Data with Num[T]](val gen:HardType[T],clip:Boolean = true) extends SloanModule{

  val io = new Bundle{
    val load =  in Bool()
    val mulInput = in(gen())
    val addInput = in(gen())

    val passthrough = out(gen())
    val macOut = out(gen)
  }

  val weight = Reg(gen()).init(zero(gen.craft()))
  val macOut = Reg(gen()).init(zero(gen.craft()))

  io.passthrough := RegNext(io.mulInput).init(zero(gen.craft()))

  when(io.load){ /* load the weight*/
    weight := io.addInput
    io.macOut := weight
  }.otherwise{
    val macTemp = if(clip) upDown(mac(gen.craft(),io.mulInput,weight,io.addInput),gen()).resized else mac(gen.craft(),io.mulInput,weight,io.addInput)
    macOut := macTemp
    io.macOut := macOut
  }
}

object MAC extends App{
  import spinal.core.sim._
  SIMCFG().compile{
    val dut = new MAC(SInt( 8 bits))
    dut
  }.doSimUntilVoid{
    dut =>
      dut.clockDomain.forkStimulus(10)
      /* with clip */
      def clipValue(width:Int,sign:Boolean,value:Int) = {
        val max = if(sign) Math.pow(2,width - 1) - 1 else Math.pow(2,width) - 1
        val min = if(sign) -Math.pow(2,width - 1) else 0
        val clip = if(value > max){max.toInt}
        else if(value < min){min.toInt}
        else{value}
        clip
      }
      def width = 8
      def sign = true
      def testCase = 1024
      val weight = Array.fill(testCase){Random.nextInt(100) - 50}
      val adds = Array.fill(testCase){Random.nextInt(100) - 50}
      val activation = Array.fill(testCase){Random.nextInt(100) - 50}
      dut.io.load #= false
      dut.io.mulInput #= 0
      dut.io.addInput #= 0
      var idx = 0
      dut.clockDomain.waitSampling()
      val Mac = fork{
        // first load
        while (true){
          dut.io.load #= true
          dut.io.addInput #= weight(idx)
          dut.clockDomain.waitSampling()
          dut.io.mulInput #= activation(idx)
          dut.io.addInput #= adds(idx)
          dut.clockDomain.waitSampling()
          val refMac = weight(idx) * activation(idx) + adds(idx)
          assert(dut.io.macOut.toBigInt == clipValue(width, sign, refMac),s"${dut.io.macOut.toBigInt} is not match ${clipValue(width, sign, refMac)}")
          idx += 1
          if(idx == testCase){simSuccess()}
        }

      }

  }
}