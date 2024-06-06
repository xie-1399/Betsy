/* SPDX-License-Identifier: Apache-2.0 */
/* Copyright © 2019-2022 Tensil AI Company */

package Slogan.src

import tensil.Architecture
import tensil.tools.CompilerOptions

class IsolatedLocalSchedulingContext(options: CompilerOptions)
    extends SchedulingContext(options) {

  override def mkScheduler(layerIndex: Int): Scheduler =
    new IsolatedLocalScheduler(
      layerIndex,
      this
    )
}
