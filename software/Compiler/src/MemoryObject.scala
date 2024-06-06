/* SPDX-License-Identifier: Apache-2.0 */
/* Copyright © 2019-2022 Tensil AI Company */

package Slogan.src

case class MemoryObject(
    name: String,
    span: MemorySpan,
    dims: MemoryDimensions
) {
  def mkAddress(offset: Int): MemoryAddress =
    span(offset)

  def mkSub(
      name: String,
      offset: Int,
      dims: MemoryDimensions
  ): MemoryObject = {
    val subAddresses = span.slice(offset, (offset + dims.sizeVectors)).toArray

    MemoryObject(
      name,
      subAddresses,
      dims
    )
  }
}

case class MemoryOptionalInputOutputObjects(
    input: Option[MemoryObject],
    output: MemoryObject
) {}
