package org.milllang.millc

sealed abstract class Symbol
case class MemberValueSymbol(module: ModuleName, member: String) extends Symbol
