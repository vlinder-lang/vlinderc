package org.milllang.millc.name

import org.milllang.millc.ModuleName

sealed abstract class Symbol
case class MemberValueSymbol(module: ModuleName, member: String) extends Symbol
