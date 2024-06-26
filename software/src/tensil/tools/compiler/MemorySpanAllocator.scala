/* SPDX-License-Identifier: Apache-2.0 */
/* Copyright © 2019-2022 Tensil AI Company */

package tensil.tools.compiler

import scala.collection.mutable

import tensil.tools.CompilerException
import tensil.common.TablePrinter

class MemorySpanAllocator() {
  private val allocatedSpans = mutable.Map.empty[MemoryRef, MemorySpan]
  private val blendedSets =
    mutable.ListBuffer.empty[mutable.Set[MemoryRef]]

  def reportSpans(): Unit = {
    val tp = new TablePrinter(Some("MEMORY SPANS"))

    for ((ref, allocatedSpan) <- allocatedSpans.toSeq.sortBy(_._2.head.raw))
      tp.addNamedLine(
        ref.toString(),
        allocatedSpan.size,
        allocatedSpan.grouped(8).map(_.mkString(",")).toIndexedSeq
      )

    print(tp)
  }

  def allocate(
      space: MemorySpace,
      ref: MemoryRef,
      size: MemoryAddressRaw
  ): MemorySpan = {
    space.allocate(ref, size) match {
      case Some(allocatedSpan) =>
        allocatedSpans(ref) = allocatedSpan
        allocatedSpan

      case None =>
        throw new CompilerException(
          s"Insufficient ${space.name} memory to allocate ${ref} of size ${size}"
        )
    }
  }

  def blend(
      ref: MemoryRef,
      blendeeRefs: Seq[MemoryRef],
      blendedSpan: MemorySpan
  ): MemorySpan = {
    val newSet      = (blendeeRefs :+ ref).toSet
    val existingSet = blendedSets.find(set => !(newSet & set).isEmpty)

    if (existingSet.isDefined)
      existingSet.get ++= newSet
    else
      blendedSets += newSet.to[mutable.Set]

    allocatedSpans(ref) = blendedSpan

    blendedSpan
  }

  def free(spaces: Seq[MemorySpace], ref: MemoryRef): MemorySpan = {
    val span = allocatedSpans.remove(ref) match {
      case Some(span) => span
      case None =>
        throw new CompilerException(
          s"Unresolved memory span ${ref}"
        )
    }

    var mayFree = true

    for (set <- blendedSets)
      if (set.remove(ref))
        if (set.isEmpty)
          blendedSets -= set
        else
          mayFree = false

    if (mayFree)
      spaces.foreach(_.free(span))

    span
  }
}
