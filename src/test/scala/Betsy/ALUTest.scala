package Betsy

import Betsy.SimTools._
import BetsyLibs._
import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._

import scala.util.Random

class ALUTest extends AnyFunSuite {

  test("without insert register ") {
    SIMCFG().compile {
      val dut = new ALU(SInt(8 bits), numOps = 16, numRegisters = 1) /* no insert registers */
      dut.registers.simPublic()
      dut
    }.doSimUntilVoid {
      dut =>
        SimTimeout(10 us)
        dut.clockDomain.forkStimulus(10)
        def testCase = 4096 * 4
        def bitWidth = 8
        val sign = true

        def ALUInit(): Unit = {
          dut.io.input #= 0
          dut.io.op #= 0
          dut.io.dest #= 0
          dut.io.sourceLeft #= 0
          dut.io.sourceRight #= 0
          dut.clockDomain.waitSampling()
        }

        ALUInit()
        val inputs = if (sign) Array.fill(testCase) {Random.nextInt(255) - 128}
        else {Array.fill(testCase) {Random.nextInt(256)}}

        for (idx <- 0 until testCase) {
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

          val sourceLeftValue = if (randLeft == 0) {inputs(idx)} else {dut.registers(0).toInt}
          val sourceRightValue = if (randRight == 0) {inputs(idx)} else {dut.registers(0).toInt}
          val leftBoolValue = sourceLeftValue != 0
          val rightBoolValue = sourceRightValue != 0
          val greater = sourceLeftValue > sourceRightValue
          val greaterThan = sourceLeftValue >= sourceRightValue
          val min = if (sourceLeftValue > sourceRightValue) {sourceRightValue} else {sourceLeftValue}
          val max = if (sourceLeftValue > sourceRightValue) {sourceLeftValue} else {sourceRightValue}

          randOp match {
            case 0 => assert(dut.io.output.toInt == inputs(idx), "NoOp error !!!")
            case 1 => assert(dut.io.output.toInt == 0, "Zero error !!!")
            case 2 => assert(dut.io.output.toInt == sourceLeftValue, "Move error !!!")
            case 3 => assert(dut.io.output.toInt == (!leftBoolValue).toInt, "Not error !!!")
            case 4 => assert(dut.io.output.toInt == (leftBoolValue && rightBoolValue).toInt, "And error !!!")
            case 5 => assert(dut.io.output.toInt == (leftBoolValue || rightBoolValue).toInt, "Or error !!!")
            case 6 => assert(dut.io.output.toInt == clipValue(bitWidth, sign, sourceLeftValue + 1), "Increment error !!!")
            case 7 => assert(dut.io.output.toInt == clipValue(bitWidth, sign, sourceLeftValue - 1), "Decrement error !!!")
            case 8 => assert(dut.io.output.toInt == clipValue(bitWidth, sign, sourceLeftValue + sourceRightValue), s"${dut.io.output.toInt} /= ${clipValue(bitWidth, true, sourceLeftValue + sourceRightValue)} -> Add error !!!")
            case 9 => assert(dut.io.output.toInt == clipValue(bitWidth, sign, sourceLeftValue - sourceRightValue), s"${dut.io.output.toInt} /= ${clipValue(bitWidth, sign, sourceLeftValue - sourceRightValue)} -> Sub error !!!")
            case 10 => assert(dut.io.output.toInt == clipValue(bitWidth, sign, sourceLeftValue * sourceRightValue), "Mul error !!!")
            case 11 =>
              if (sourceLeftValue.abs != math.pow(2, bitWidth - 1)) {
                assert(dut.io.output.toInt == (sourceLeftValue.abs), s"${dut.io.output.toInt} /= ${(sourceLeftValue.abs)} -> Abs error !!!")
              }else{
                assert(dut.io.output.toInt == math.pow(2, bitWidth - 1) - 1)
              }
            case 12 => assert(dut.io.output.toInt == greater.toInt, "greater error !!!")
            case 13 => assert(dut.io.output.toInt == greaterThan.toInt, "greater than error !!!")
            case 14 => assert(dut.io.output.toInt == min, "min error !!!")
            case 15 => assert(dut.io.output.toInt == max, "max error !!!")
            case _ =>
          }
        }
        simSuccess()
    }
  }

  test("AFix ") {
    SIMCFG().compile {
      val dut = new ALU(AFix(4 exp, -2 exp, true), numOps = 16, numRegisters = 1) /* no insert registers */
      dut.registers.simPublic()
      dut
    }.doSimUntilVoid {
      dut =>
        SimTimeout(10 us)
        dut.clockDomain.forkStimulus(10)
        def testCase = 4096 * 4
        def intWidth = 5
        def fracWidth = -2
        val sign = true

        def ALUInit(): Unit = {
          dut.io.input #= 0
          dut.io.op #= 0
          dut.io.dest #= 0
          dut.io.sourceLeft #= 0
          dut.io.sourceRight #= 0
          dut.clockDomain.waitSampling()
        }

        ALUInit()
        val inputs = if (sign) Array.fill(testCase) {0.25 * Random.nextInt(127) - 16}
        else {Array.fill(testCase) {0.25 * Random.nextInt(127) - 16}}

        for (idx <- 0 until testCase) {
          val randOp = 6 + Random.nextInt(10)
          val randLeft = Random.nextInt(2)
          val randRight = Random.nextInt(2)
          val randDest = Random.nextInt(2)
          dut.io.op #= randOp
          dut.io.sourceLeft #= randLeft
          dut.io.sourceRight #= randRight
          dut.io.dest #= randDest
          dut.io.input #= inputs(idx)
          dut.clockDomain.waitSampling()

          val sourceLeftValue = if (randLeft == 0) {inputs(idx)} else {dut.registers(0).toDouble}
          val sourceRightValue = if (randRight == 0) {inputs(idx)} else {dut.registers(0).toDouble}
          val min = if (sourceLeftValue > sourceRightValue) {sourceRightValue} else {sourceLeftValue}
          val max = if (sourceLeftValue > sourceRightValue) {sourceLeftValue} else {sourceRightValue}
          val greater = sourceLeftValue > sourceRightValue
          val greaterThan = sourceLeftValue >= sourceRightValue
          randOp match {
            case 0 => assert(compareDouble(dut.io.output.toDouble, inputs(idx), -2), "NoOp error !!!")
            case 1 => assert(compareDouble(dut.io.output.toDouble, 0, -2), "Zero error !!!")
            case 2 => assert(compareDouble(dut.io.output.toDouble, sourceLeftValue, -2), "Move error !!!")
            case 6 => assert(compareDouble(dut.io.output.toDouble, clipValueForDouble(15.75, -16.00, sourceLeftValue + 1), 0), "Increment error !!!")
            case 7 => assert(compareDouble(dut.io.output.toDouble, clipValueForDouble(15.75, -16.00, sourceLeftValue - 1), 0), "Decrement error !!!")
            case 8 => assert(compareDouble(dut.io.output.toDouble, clipValueForDouble(15.75, -16.00, sourceLeftValue + sourceRightValue), 0), s"${dut.io.output.toDouble} /= ${clipValueForDouble(15.75, -16.00, sourceLeftValue + sourceRightValue)} -> Add error !!!")
            case 9 => assert(compareDouble(dut.io.output.toDouble, clipValueForDouble(15.75, -16.00, sourceLeftValue - sourceRightValue), 0), s"${dut.io.output.toDouble} /= ${clipValueForDouble(15.75, -16.00, sourceLeftValue - sourceRightValue)} -> Sub error !!!")
            case 10 => assert(compareDouble(dut.io.output.toDouble, clipValueForDouble(15.75, -16.00, sourceLeftValue * sourceRightValue), -2), "Mul error !!!")
            case 11 =>
              if (sourceLeftValue.abs != math.pow(2, intWidth - 1)) {
                assert(dut.io.output.toDouble == (sourceLeftValue.abs), s"${dut.io.output.toDouble} /= ${(sourceLeftValue.abs)} -> Abs error !!!")
              }else{
                assert(dut.io.output.toDouble == math.pow(2, intWidth - 1) - math.pow(2, fracWidth))
              }
            case 12 => assert(dut.io.output.toDouble == greater.toInt, "greater error !!!")
            case 13 => assert(dut.io.output.toDouble == greaterThan.toInt, "greater than error !!!")
            case 14 => assert(dut.io.output.toDouble == min, "min error !!!")
            case 15 => assert(dut.io.output.toDouble == max, "max error !!!")
            case _ =>
          }
        }
        simSuccess()
    }
  }

}
