package org.vlinderlang.vlinderc.name

import org.vlinderlang.vlinderc.ModuleName

sealed abstract class Symbol
case class MemberValueSymbol(module: ModuleName, member: String) extends Symbol
