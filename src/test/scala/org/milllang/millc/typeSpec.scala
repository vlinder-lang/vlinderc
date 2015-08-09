package org.milllang.millc

import org.scalatest._

class typeSpec extends FlatSpec {
  "equal" should "compare two types for equality" in {
    implicit val context = Context(
      Map(
        (ModuleName("mill", "log"), "Record") -> StructTypeDecl("Record", Vector()),
        (ModuleName("mill", "log"), "Level") -> UnionTypeDecl("Level", Vector()),
        (ModuleName("mill", "log"), "Logger") -> AliasTypeDecl("Logger", TupleType())
      ),
      Map()
    )

    assert(StringType equal StringType)
    assert(TupleType() equal TupleType())
    assert(!(StringType equal TupleType()))
    assert(NamedType((ModuleName("mill", "log"), "Record")) equal
           NamedType((ModuleName("mill", "log"), "Record")))
    assert(NamedType((ModuleName("mill", "log"), "Logger")) equal
           NamedType((ModuleName("mill", "log"), "Logger")))
    assert(NamedType((ModuleName("mill", "log"), "Logger")) equal
           TupleType())
    assert(!(NamedType((ModuleName("mill", "log"), "Logger")) equal
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
    assert(NamedType((ModuleName("mill", "log"), "Record")).descriptor == "Nmill.log.Record;")
  }

  "unify" should "succeed for equal types" in {
    implicit val context = Context(Map(), Map())
    Type.unify(StringType, StringType)
  }

  it should "unify concrete types with variable types" in {
    implicit val context = Context(Map(), Map())
    val variableType = VariableType()
    Type.unify(variableType, StringType)
    assert(variableType equal StringType)
  }

  it should "fail for different types" in {
    implicit val context = Context(Map(), Map())
    intercept[TypeError] {
      Type.unify(StringType, TupleType())
    }
    intercept[TypeError] {
      val variableType = VariableType()
      Type.unify(variableType, StringType)
      Type.unify(variableType, TupleType())
    }
  }
}
