package Betsy

import spinal.core.sim._
import scala.util.Random
import Betsy.SimTools._
import BetsyLibs._
import spinal.core._

object ALUTest extends App{

  /* the register pipe tested later ...*/
  /* Todo with abs boundary QAQ */

  SIMCFG().compile{
    val dut = new ALU(SInt(8 bits),numOps = 16,numRegisters = 1) /* no insert registers */
    dut.registers.simPublic()
    dut
  }.doSimUntilVoid{
    dut =>
      dut.clockDomain.forkStimulus(10)
      def testCase = 4096
      def bitWidth = 8
      val sign = true
      def ALUInit() = {
        dut.io.input #= 0
        dut.io.op #= 0
        dut.io.dest #= 0
        dut.io.sourceLeft #= 0
        dut.io.sourceRight #= 0
        dut.clockDomain.waitSampling()
      }
      ALUInit()

      val inputs = if(sign) Array.fill(testCase){Random.nextInt(120) - 120} else {Array.fill(testCase){Random.nextInt(256)}}
      for(idx <- 0 until testCase){
        val randOp = Random.nextInt(16)
        val randLeft = Random.nextInt(2)
        val randRight = Random.nextInt(2)
        val randDest = Random.nextInt(2)
        dut.io.op #= randOp
        dut.io.sourceLeft #= randLeft
        dut.io.sourceRight #= randRight
        dut.io.dest #= randDest
        dut.io.input #= inputs(idx)
        dut.clockDomain.waitSampling()

        val sourceLeftValue = if(randLeft == 0){inputs(idx)} else {dut.registers(0).toInt}
        val sourceRightValue = if(randRight == 0){inputs(idx)} else {dut.registers(0).toInt}
        val leftBoolValue = if(sourceLeftValue != 0){true} else {false}
        val rightBoolValue = if(sourceRightValue != 0){true} else {false}
        val greater = if(sourceLeftValue > sourceRightValue){true}else{false}
        val greaterThan = if(sourceLeftValue >= sourceRightValue){true}else{false}
        val min = if(sourceLeftValue > sourceRightValue){sourceRightValue}else{sourceLeftValue}
        val max = if(sourceLeftValue > sourceRightValue){sourceLeftValue}else{sourceRightValue}

        randOp match {
          case 0 => assert(dut.io.output.toInt == inputs(idx),"NoOp error !!!")
          case 1 => assert(dut.io.output.toInt == 0,"Zero error !!!")
          case 2 => assert(dut.io.output.toInt == sourceLeftValue,"Move error !!!")
          case 3 => assert(dut.io.output.toInt == (!leftBoolValue).toInt,"Not error !!!")
          case 4 => assert(dut.io.output.toInt == (leftBoolValue && rightBoolValue).toInt,"And error !!!")
          case 5 => assert(dut.io.output.toInt == (leftBoolValue || rightBoolValue).toInt,"Or error !!!")
          case 6 => assert(dut.io.output.toInt == clipValue(bitWidth,sign,sourceLeftValue + 1),"Increment error !!!")
          case 7 => assert(dut.io.output.toInt == clipValue(bitWidth,sign,sourceLeftValue - 1),"Decrement error !!!")
          case 8 => assert(dut.io.output.toInt == clipValue(bitWidth,sign,sourceLeftValue + sourceRightValue),s"${dut.io.output.toInt} /= ${clipValue(bitWidth,true,sourceLeftValue + sourceRightValue)} -> Add error !!!")
          case 9 => assert(dut.io.output.toInt == clipValue(bitWidth,sign,sourceLeftValue - sourceRightValue),s"${dut.io.output.toInt} /= ${clipValue(bitWidth,sign,sourceLeftValue - sourceRightValue)} -> Sub error !!!")
          case 10 => assert(dut.io.output.toInt == clipValue(bitWidth,sign,sourceLeftValue * sourceRightValue),"Mul error !!!")
          // case 11 => assert(dut.io.output.toInt == (sourceLeftValue.abs),s"${dut.io.output.toInt} /= ${(sourceLeftValue.abs)} -> Abs error !!!")
          case 12 => assert(dut.io.output.toInt == greater.toInt,"greater error !!!")
          case 13 => assert(dut.io.output.toInt == greaterThan.toInt,"greater than error !!!")
          case 14 => assert(dut.io.output.toInt == min,"min error !!!")
          case 15 => assert(dut.io.output.toInt == max,"max error !!!")
          case _ =>
        }
      }
      simSuccess()
  }

}
