/* SPDX-License-Identifier: Apache-2.0 */
/* Copyright © 2019-2022 Tensil AI Company */

package tensil.tools.compiler.lir

import scala.collection.mutable

import tensil.tools.compiler.{
  LIR,
  InstructionContext,
  MemoryAddress,
  MemoryAddressHelper,
  MemoryAddressRaw,
  MemoryTag,
  Stats,
  Estimator,
  Estimate,
  Opcode,
  DataMoveFlags
}
import tensil.common.Architecture

class StatsGen(
    arch: Architecture,
    stats: Stats
) extends LIR {
  private val estimator = new Estimator(arch)
  private val estimateQueues =
    Array.fill(arch.numberOfThreads)(mutable.Queue.empty[Estimate])

  private def countExecution(maxQueueSize: Int): Unit = {
    val nonEmptyTids =
      estimateQueues.map(_.size).zipWithIndex.filter(_._1 != 0).map(_._2)

    val estimate = if (nonEmptyTids.size > 1) {
      val minCycles =
        nonEmptyTids.map(estimateQueues(_).front.cycles).min
      val estimates =
        for (tid <- nonEmptyTids)
          yield
            if (estimateQueues(tid).front.cycles == minCycles)
              estimateQueues(tid).dequeue()
            else
              estimateQueues(tid).front.splitCycles(minCycles)

      Some(
        estimates.reduce((a, b) => new Estimate(a.cycles, a.energy + b.energy))
      )

    } else if (nonEmptyTids.size == 1) {
      val queue = estimateQueues(nonEmptyTids(0))

      if (queue.size > maxQueueSize) {
        val dequeueSize = queue.size - maxQueueSize
        val estimates   = for (_ <- 0 until dequeueSize) yield queue.dequeue()

        Some(
          estimates.reduce((a, b) =>
            new Estimate(a.cycles + b.cycles, a.energy + b.energy)
          )
        )
      } else
        None
    } else None

    if (estimate.isDefined)
      stats.countExecution(estimate.get)
  }

  private def count(
      tid: Int,
      mnemonic: String,
      estimate: Estimate,
      size: Long = 0
  ): Unit = {
    stats.countInstruction(
      mnemonic,
      estimate,
      size
    )

    estimateQueues(tid).enqueue(estimate)

    countExecution(arch.threadQueueDepth + 1)
  }

  def emitWait(
      tidToWait: Int,
      tid: Int,
      context: Option[InstructionContext]
  ): Unit = {
    count(
      tid,
      "Wait",
      estimator.estimateCyclesAndEnergy(Opcode.Wait)
    )
  }

  def emitMatMul(
      accumulate: Boolean,
      localStride: Int,
      localAddress: MemoryAddress,
      accumulatorStride: Int,
      accumulatorAddress: MemoryAddress,
      size: MemoryAddressRaw,
      tid: Int,
      context: Option[InstructionContext]
  ): Unit = {
    val mnemonic = "MatMul"

    count(
      tid,
      mnemonic,
      estimator.estimateCyclesAndEnergy(Opcode.MatMul, Some(size)),
      size
    )
    if (localAddress.tag != MemoryTag.Zeroes)
      stats.countStride(mnemonic, 0, localStride, size)
    stats.countStride(mnemonic, 1, accumulatorStride, size)
  }

  def emitSIMD(
      accumulate: Boolean,
      simdOp: Int,
      simdSourceLeft: Int,
      simdSourceRight: Int,
      simdDestination: Int,
      writeAccumulatorAddress: MemoryAddress,
      readAccumulatorAddress: MemoryAddress,
      tid: Int,
      context: Option[InstructionContext]
  ): Unit = {
    count(
      tid,
      "SIMD",
      estimator.estimateCyclesAndEnergy(Opcode.SIMD)
    )
  }

  def emitDataMove(
      toLocal: Boolean,
      accumulate: Boolean,
      localStride: Int,
      localAddress: MemoryAddress,
      stride: Int,
      address: MemoryAddress,
      size: MemoryAddressRaw,
      tid: Int,
      context: Option[InstructionContext]
  ): Unit = {
    val flags = StreamGen.mkDataMoveFlags(toLocal, accumulate, address.tag)
    val suffix = flags match {
      case DataMoveFlags.LocalToDRAM0       => "(LocalToDRAM0)"
      case DataMoveFlags.LocalToDRAM1       => "(LocalToDRAM1)"
      case DataMoveFlags.DRAM0ToLocal       => "(DRAM0ToLocal)"
      case DataMoveFlags.DRAM1ToLocal       => "(DRAM1ToLocal)"
      case DataMoveFlags.AccumulatorToLocal => "(AccToLocal)"
      case DataMoveFlags.LocalToAccumulator |
          DataMoveFlags.LocalToAccumulatorAccumulate =>
        "(LocalToAcc)"
    }

    val mnemonic = s"DataMove${suffix}"

    count(
      tid,
      mnemonic,
      estimator.estimateCyclesAndEnergy(Opcode.DataMove, Some(size), flags),
      size
    )
    stats.countStride(mnemonic, 0, localStride, size)
    stats.countStride(mnemonic, 1, stride, size)
  }

  def emitLoadWeights(
      localStride: Int,
      localAddress: MemoryAddress,
      size: MemoryAddressRaw,
      tid: Int,
      context: Option[InstructionContext]
  ): Unit = {
    val mnemonic = "LoadWeights"

    count(
      tid,
      mnemonic,
      estimator.estimateCyclesAndEnergy(Opcode.LoadWeights, Some(size)),
      size
    )

    if (localAddress.tag != MemoryTag.Zeroes)
      stats.countStride(mnemonic, 0, localStride, size)
  }

  def endEmit(): Unit =
    while (estimateQueues.exists(!_.isEmpty))
      countExecution(0)
}
