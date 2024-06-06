/* SPDX-License-Identifier: Apache-2.0 */
/* Copyright © 2019-2022 Tensil AI Company */

package Compiler.src

import tensil.Architecture
import Compiler._

class SharedLocalSchedulingContext(
    options: CompilerOptions,
    val localSpace: MemorySpace
) extends SchedulingContext(options) {

  override def mkScheduler(layerIndex: Int): Scheduler =
    new SharedLocalScheduler(
      layerIndex,
      this
    )
}
