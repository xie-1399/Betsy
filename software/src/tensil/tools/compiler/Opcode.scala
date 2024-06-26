/* SPDX-License-Identifier: Apache-2.0 */
/* Copyright © 2019-2022 Tensil AI Company */

package tensil.tools.compiler

object Opcode {
  val Wait        = 0x0
  val MatMul      = 0x1
  val DataMove    = 0x2
  val LoadWeights = 0x3
  val SIMD        = 0x4
  // unused 0x5-0x6
  val Configure = 0x7
}
