/* SPDX-License-Identifier: Apache-2.0 */
/* Copyright © 2019-2022 Tensil AI Company */

package Slogan.src

case class EmitContext(
    hir: HIR,
    mm: MemoryManager,
    outputNames: Seq[String],
    graphPrinter: Option[FrontendGraphPrinter] = None
)
