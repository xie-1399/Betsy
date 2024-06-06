/* SPDX-License-Identifier: Apache-2.0 */
/* Copyright © 2019-2022 Tensil AI Company */

package Slogan.src

import tensil.Architecture
import tensil.tools.CompilerOptions

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
