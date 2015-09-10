package org.vlinderlang.vlinderc

package object module2yaml {
  import com.google.gson.{GsonBuilder, JsonArray, JsonElement, JsonObject, JsonPrimitive}
  import java.util.Locale

  def convert(module: ast.Module, cfgs: Map[(ModuleName, String), ssa.CFG]): String = {
    val gson = new GsonBuilder().setPrettyPrinting().create()

    val root = new JsonObject()

    root.add("name", new JsonPrimitive(module.name.segments.mkString(".")))

    val imports = new JsonArray()
    val structs = new JsonArray()
    val unions = new JsonArray()
    val aliases = new JsonArray()
    val subs = new JsonArray()
    module.decls foreach {
      case ast.ImportDecl(importee) =>
        imports.add(new JsonPrimitive(importee.segments.mkString(".")))

      case ast.StructDecl(name, structFields) =>
        val struct = new JsonObject()
        struct.add("name", new JsonPrimitive(name))
        val fields = new JsonArray()
        for ((fieldName, fieldType) <- structFields) {
          val field = new JsonObject()
          field.add("name", new JsonPrimitive(fieldName))
          field.add("type", new JsonPrimitive(`type`.typeExprToType(fieldType).descriptor))
          fields.add(field)
        }
        struct.add("fields", fields)
        structs.add(struct)

      case ast.UnionDecl(name, unionConstructors) =>
        val union = new JsonObject()
        union.add("name", new JsonPrimitive(name))
        val constructors = new JsonArray()
        for ((constructorName, _) <- unionConstructors) {
          val constructor = new JsonObject()
          constructor.add("name", new JsonPrimitive(constructorName))
          constructor.add("parameters", new JsonArray())
          constructors.add(constructor)
        }
        union.add("constructors", constructors)
        unions.add(union)

      case ast.AliasDecl(name, underlyingType) =>
        val alias = new JsonObject()
        alias.add("name", new JsonPrimitive(name))
        alias.add("type", new JsonPrimitive(`type`.typeExprToType(underlyingType).descriptor))
        aliases.add(alias)

      case ast.SubDecl(name, valueParams, returnType, body) =>
        val sub = new JsonObject()

        sub.add("name", new JsonPrimitive(name))

        val parameters = new JsonArray()
        for ((paramName, paramType) <- valueParams) {
          val parameter = new JsonObject()
          parameter.add("name", new JsonPrimitive(paramName))
          parameter.add("type", new JsonPrimitive(`type`.typeExprToType(paramType).descriptor))
          parameters.add(parameter)
        }
        sub.add("parameters", parameters)

        sub.add("localCount", new JsonPrimitive(1024)) // TODO: Actual local count.

        val insts = ssa2bc.convert(cfgs((module.name, name)))
        sub.add("body", {val a = new JsonArray(); insts.map(convertInst) foreach a.add; a})

        subs.add(sub)
    }
    root.add("imports", imports)
    root.add("structs", structs)
    root.add("unions", unions)
    root.add("aliases", aliases)
    root.add("subs", subs)
    root.add("foreignSubs", new JsonArray())

    gson.toJson(root)
  }

  def convertInst(inst: ssa2bc.Inst): JsonElement = {
    val obj = new JsonObject()

    val opcode = inst.getClass.getSimpleName.toLowerCase(Locale.ENGLISH).replaceAll("inst\\$?$", "")
    obj.add("opcode", new JsonPrimitive(opcode))

    inst.getClass.getMethods.toVector
    .filter(_.getName.startsWith("VLINDER"))
    .foreach{ method =>
      val name = method.getName.substring("VLINDER".size)
      val value = method.invoke(inst) match {
        case v: Integer => new JsonPrimitive(v)
        case v: String => new JsonPrimitive(v)
      }
      obj.add(name, value)
    }

    return obj
  }
}
