/* SPDX-License-Identifier: Apache-2.0 */
/* Copyright © 2019-2022 Tensil AI Company */

package compiler.src

// import tensil.Architecture

class Estimate(initCycles: Long, initEnergy: Long) {
  private var curCycles = initCycles
  private var curEnergy = initEnergy

  def cycles = curCycles
  def energy = curEnergy

  def splitCycles(cyclesToSplit: Long): Estimate = {
    require(cyclesToSplit < cycles)

    val energyToSplit =
      ((cyclesToSplit.toFloat / cycles.toFloat) * energy.toFloat).toLong

    curCycles -= cyclesToSplit
    curEnergy -= energyToSplit

    new Estimate(cyclesToSplit, energyToSplit)
  }
}

class Estimator(arch: Architecture) {
  private var previousOpcode = Opcode.Wait
  private var previousFlags  = 0

  private val InternalTransferEnergy = 10
  private val InternalTransferCycles = 1

  private val DRAMTransferEnergy = 100
  private val DRAMTransferCycles = 1

  /*
  private val DRAMTransferSetupReadCycles  = 32
  private val DRAMTransferSetupWriteCycles = 0
  private val DRAMTransferReadCycles       = 2
  private val DRAMTransferWriteCycles      = 1
  private val DRAMTransferWidthBits        = 128
   */

  def estimateCyclesAndEnergy(
      currentOp: Int,
      size: Option[MemoryAddressRaw] = None,
      flags: Int = 0
  ): Estimate = {
    val r = currentOp match {
      case Opcode.Wait =>
        val cycles = 1L
        val energy = 0L

        new Estimate(cycles, energy)
      case Opcode.MatMul => {
        val cycles =
          (if (previousOpcode == Opcode.MatMul)
             (size.get + 1)
           else if (previousOpcode == Opcode.LoadWeights)
             (size.get + 1 + arch.arraySize)
           else
             (size.get + 1 + 2 * arch.arraySize))

        val energy =
          (size.get + 1) * arch.arraySize * arch.arraySize

        new Estimate(cycles, energy)
      }

      case Opcode.SIMD => {
        val cycles = 1L
        val energy = arch.arraySize.toLong

        new Estimate(cycles, energy)
      }

      case Opcode.LoadWeights => {
        val cycles = (size.get + 1) * InternalTransferCycles
        val energy = (size.get + 1) * InternalTransferEnergy

        new Estimate(cycles, energy)
      }

      case Opcode.DataMove =>
        if (
          flags == DataMoveFlags.LocalToDRAM0 ||
          flags == DataMoveFlags.LocalToDRAM1 ||
          flags == DataMoveFlags.DRAM0ToLocal ||
          flags == DataMoveFlags.DRAM1ToLocal
        ) {
          /*val transfersPerVector = divCeil(
            dataType.sizeBytes * arch.arraySize * 8,
            DRAMTransferWidthBits
          )
          val isRead =
            flags == DataMoveFlags.DRAM0ToLocal || flags == DataMoveFlags.DRAM1ToLocal
          val transferCycles =
            if (isRead) DRAMTransferReadCycles
            else DRAMTransferWriteCycles
          val setupCycles =
            if (previousOpcode == Opcode.DataMove && previousFlags == flags) 0
            else if (isRead) DRAMTransferSetupReadCycles
            else DRAMTransferSetupWriteCycles

          val cycles =
            (size.get + 1) * transfersPerVector * transferCycles + setupCycles
          val energy = (size.get + 1) * transfersPerVector * DRAMTransferEnergy*/

          val cycles = (size.get + 1) * DRAMTransferCycles
          val energy = (size.get + 1) * DRAMTransferEnergy

          new Estimate(cycles, energy)
        } else {
          val cycles = (size.get + 1) * InternalTransferCycles
          val energy = (size.get + 1) * InternalTransferEnergy

          new Estimate(cycles, energy)
        }

      case _ =>
        new Estimate(0L, 0L)
    }

    previousOpcode = currentOp
    previousFlags = flags

    r
  }
}
