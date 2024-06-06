/* SPDX-License-Identifier: Apache-2.0 */
/* Copyright © 2019-2022 Tensil AI Company */

package Slogan.emulator

import tensil.tools.compiler.{MemoryAddressRaw}

trait Executive {
  def peekAccumulator(address: MemoryAddressRaw): Array[Float]
  def peekLocal(address: MemoryAddressRaw): Array[Float]
  def peekDRAM0(address: MemoryAddressRaw): Array[Float]
  def peekDRAM1(address: MemoryAddressRaw): Array[Float]
}
