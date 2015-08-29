package org.vlinderlang.vlinderc.opt

import org.vlinderlang.vlinderc.ssa.CFG

trait Optimization extends (CFG => CFG) {
  def name: String

  def breaking: Boolean
}
