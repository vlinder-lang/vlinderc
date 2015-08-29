package org.vlinderlang.vlinderc.name

import org.vlinderlang.vlinderc.ModuleName

sealed abstract class Symbol
case class ImportSymbol(module: ModuleName) extends Symbol
case class MemberTypeSymbol(module: ModuleName, member: String) extends Symbol
case class MemberValueSymbol(module: ModuleName, member: String) extends Symbol
case class ValueParamSymbol(name: String) extends Symbol
case object StringTypeSymbol extends Symbol
