package org.vlinderlang.vlinderc.`type`

import org.vlinderlang.vlinderc.ast._
import org.vlinderlang.vlinderc.ModuleName
import org.vlinderlang.vlinderc.name.MemberValueSymbol
import org.scalatest.FlatSpec

class packageSpec extends FlatSpec {
  private implicit val canSetSymbol = CanSetSymbol

  "equal" should "compare two types for equality" in {
    implicit val context = Context(
      Map(
        (ModuleName("vlinder", "log"), "Record") -> StructTypeDecl("Record", Vector()),
        (ModuleName("vlinder", "log"), "Level") -> UnionTypeDecl("Level", Vector()),
        (ModuleName("vlinder", "log"), "Logger") -> AliasTypeDecl("Logger", TupleType())
      ),
      Map()
    )

    assert(StringType equal StringType)
    assert(TupleType() equal TupleType())
    assert(!(StringType equal TupleType()))
    assert(NamedType((ModuleName("vlinder", "log"), "Record")) equal
           NamedType((ModuleName("vlinder", "log"), "Record")))
    assert(NamedType((ModuleName("vlinder", "log"), "Logger")) equal
           NamedType((ModuleName("vlinder", "log"), "Logger")))
    assert(NamedType((ModuleName("vlinder", "log"), "Logger")) equal
           TupleType())
    assert(!(NamedType((ModuleName("vlinder", "log"), "Logger")) equal
             StringType))
  }

  "descriptor" should "return the correct type descriptor" in {
    assert(StringType.descriptor == "S")
    assert(TupleType().descriptor == "T;")
    assert(TupleType(StringType, StringType).descriptor == "TSS;")
    assert(TupleType(StringType, TupleType()).descriptor == "TST;;")
    assert(SubType(Vector.empty, StringType).descriptor == "FS;")
    assert(SubType(Vector(StringType, StringType), StringType).descriptor == "FSSS;")
    assert(SubType(Vector(StringType, TupleType()), StringType).descriptor == "FST;S;")
    assert(NamedType((ModuleName("vlinder", "log"), "Record")).descriptor == "Nvlinder.log.Record;")
  }

  "analyze" should "succeed for empty block exprs" in {
    implicit val context = Context(Map(), Map())
    val expr = BlockExpr()
    analyze(expr)
    assert(expr.`type` equal TupleType())
  }

  it should "succeed when all but the last expr in a block exprs are not of type ()" in {
    implicit val context = Context(Map(), Map())
    val expr = BlockExpr(BlockExpr(), BlockExpr(), StringLiteralExpr("ok"))
    analyze(expr)
    assert(expr.`type` equal StringType)
  }

  it should "fail when any but the last expr in a block exprs is not of type ()" in {
    implicit val context = Context(Map(), Map())
    intercept[TypeError] {
      analyze(BlockExpr(StringLiteralExpr("bad"), StringLiteralExpr("ok")))
    }
  }

  it should "succeed when calling subroutines with correct argument types" in {
    implicit val context = Context(
      Map(),
      Map(
        (ModuleName("vlinder", "debug"), "trace") -> SubType(Vector(StringType), TupleType())
      )
    )
    val callee = NameExpr(QualifiedName("debug", "trace"))
    callee.symbol = MemberValueSymbol(ModuleName("vlinder", "debug"), "trace")
    val call = CallExpr(callee, Vector(StringLiteralExpr("Hello, world!")))
    analyze(call)
    assert(call.`type` equal TupleType())
  }

  it should "fail when calling subroutines with wrong argument types" in {
    implicit val context = Context(
      Map(),
      Map(
        (ModuleName("vlinder", "debug"), "trace") -> SubType(Vector(StringType), TupleType())
      )
    )
    val callee = NameExpr(QualifiedName("debug", "trace"))
    callee.symbol = MemberValueSymbol(ModuleName("vlinder", "debug"), "trace")
    val call = CallExpr(callee, Vector(BlockExpr()))
    intercept[TypeError] {
      analyze(call)
    }
  }

  it should "fail when calling subroutines with an invalid number of arguments" in {
    implicit val context = Context(
      Map(),
      Map(
        (ModuleName("vlinder", "debug"), "trace") -> SubType(Vector(StringType), TupleType())
      )
    )
    val callee = NameExpr(QualifiedName("debug", "trace"))
    callee.symbol = MemberValueSymbol(ModuleName("vlinder", "debug"), "trace")
    val call = CallExpr(callee, Vector())
    intercept[TypeError] {
      analyze(call)
    }
  }

  it should "fail when calling non-subroutines" in {
    implicit val context = Context(Map(), Map())
    intercept[TypeError] {
      analyze(CallExpr(BlockExpr(), Vector(StringLiteralExpr("ok"))))
    }
  }

  it should "succeed when calling generic subroutines with the correct argument types" in {
    val forallParameter = ForallParameter("T")
    implicit val context = Context(
      Map(),
      Map(
        (ModuleName("vlinder", "func"), "id") -> ForallType(
          Vector(forallParameter),
          SubType(
            Vector(ForallVariableType(forallParameter)),
            ForallVariableType(forallParameter)
          )
        )
      )
    )
    val callee = NameExpr(QualifiedName("func", "id"))
    callee.symbol = MemberValueSymbol(ModuleName("vlinder", "func"), "id")
    val call = CallExpr(callee, Vector(StringLiteralExpr("ok")))
    analyze(call)
    assert(call.`type` equal StringType)
  }

  it should "fail when calling generic subroutines with wrong argument types" in {
    val forallParameter = ForallParameter("T")
    implicit val context = Context(
      Map(),
      Map(
        (ModuleName("vlinder", "text"), "cat") -> ForallType(
          Vector(forallParameter),
          SubType(
            Vector(
              ForallVariableType(forallParameter),
              ForallVariableType(forallParameter)
            ),
            ForallVariableType(forallParameter)
          )
        )
      )
    )
    val callee = NameExpr(QualifiedName("text", "cat"))
    callee.symbol = MemberValueSymbol(ModuleName("vlinder", "text"), "cat")
    val call = CallExpr(callee, Vector(StringLiteralExpr("a"), BlockExpr()))
    intercept[TypeError] {
      analyze(call)
    }
  }

  "unify" should "succeed for equal types" in {
    implicit val context = Context(Map(), Map())
    unify(StringType, StringType)
  }

  it should "unify concrete types with variable types" in {
    implicit val context = Context(Map(), Map())
    val variableType = VariableType()
    unify(variableType, StringType)
    assert(variableType equal StringType)
  }

  it should "fail for different types" in {
    implicit val context = Context(Map(), Map())
    intercept[TypeError] {
      unify(StringType, TupleType())
    }
    intercept[TypeError] {
      val variableType = VariableType()
      unify(variableType, StringType)
      unify(variableType, TupleType())
    }
  }
}
