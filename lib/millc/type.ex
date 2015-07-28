defmodule Millc.Type do
  alias Millc.Name.ModuleSymbol, as: ModuleSymbol
  alias Millc.Name.MemberSymbol, as: MemberSymbol
  alias Millc.Name.ParamSymbol, as: ParamSymbol
  alias Millc.Name.BuiltinSymbol, as: BuiltinSymbol

  defmodule TopType, do: defstruct []
  defmodule BottomType, do: defstruct []
  defmodule StringType, do: defstruct []

  defmodule TupleType do
    @derive [Access]
    defstruct element_types: nil
  end

  defmodule SubType do
    @derive [Access]
    defstruct param_types: nil, return_type: nil
  end

  defmodule NamedType do
    @derive [Access]
    defstruct name: nil
  end

  defmodule StructDeclType do
    @derive [Access]
    defstruct name: nil, fields: nil
  end

  defmodule UnionDeclType do
    @derive [Access]
    defstruct name: nil, constructors: nil
  end

  defmodule AliasDeclType do
    @derive [Access]
    defstruct name: nil, aliases: nil
  end

  def subtype?(decl_types, a, b) do
    case {a, b} do
      {_, %TopType{}} -> true
      {%BottomType{}, _} -> true
      {a, b} -> same_type?(decl_types, a, b)
    end
  end

  def supertype?(decl_types, a, b) do
    subtype?(decl_types, b, a)
  end

  def same_type?(decl_types, a, b) do
    a = resolve_aliases(decl_types, a)
    b = resolve_aliases(decl_types, b)
    a === b
  end

  def resolve_aliases(decl_types, type) do
    case type do
      %NamedType{:name => name} ->
        case decl_types[name] do
          %AliasDeclType{:aliases => aliases} ->
            resolve_aliases(decl_types, aliases)

          _ -> type
        end

      _ -> type
    end
  end

  defmodule TypeError do
    defexception [:message]

    def exception(message) do
      %TypeError{message: message}
    end
  end

  defmodule Context do
    @derive [Access]
    defstruct decl_types: nil, member_types: nil, param_types: nil
  end

  def typecheck(modules) do
    ctx = %Context{
      :decl_types => %{},
      :member_types => %{},
      :param_types => nil,
    }

    try do
      ctx =
        modules
        |> Dict.to_list()
        |> List.foldl(ctx, fn({module_name, module}, ctx) ->
          register_decl_types(ctx, module_name, module)
        end)

      ctx =
        modules
        |> Dict.to_list()
        |> List.foldl(ctx, fn({module_name, module}, ctx) ->
          register_member_types(ctx, module_name, module)
        end)

      modules =
        modules
        |> Dict.to_list()
        |> List.foldl(%{}, fn({module_name, module}, acc) ->
          module = typecheck(ctx, module)
          Dict.put(acc, module_name, module)
        end)

      {:ok, modules}
    rescue
      e in TypeError ->
        {:error, e}
    end
  end

  defp typecheck(ctx, {:module, decls, meta}) do
    decls = Enum.map(decls, &typecheck(ctx, &1))
    {:module, decls, meta}
  end

  defp typecheck(ctx, decl = {:import_decl, _module_name, _meta}) do
    decl
  end

  defp typecheck(ctx, decl = {:struct_decl, _name, _fields, _meta}) do
    decl
  end

  defp typecheck(ctx, decl = {:union_decl, _name, _constructors, _meta}) do
    decl
  end

  defp typecheck(ctx, decl = {:alias_decl, _name, _aliases, _meta}) do
    decl
  end

  defp typecheck(ctx, {:sub_decl, name, params, return_type_expr, body, meta}) do
    ctx = put_in(ctx, [:param_types], [])
    ctx = List.foldl(params, ctx, fn({_param_name, param_type_expr}, ctx) ->
        param_type = type_expr_to_type(param_type_expr)
        update_in(ctx, [:param_types], &(&1 ++ [param_type]))
      end)
    body = typecheck(ctx, body)
    body_type = Millc.AST.meta(body)[:type]
    return_type = type_expr_to_type(return_type_expr)
    if !subtype?(ctx, body_type, return_type) do
      raise TypeError, "expected '#{format(return_type)}' but got '#{format(body_type)}'"
    end
    {:sub_decl, name, params, return_type_expr, body, meta}
  end

  defp typecheck(ctx, {:block_expr, exprs, meta}) do
    if exprs === [] do
      type = %TupleType{element_types: []}
      {:block_expr, [], Dict.put(meta, :type, type)}
    else
      init_exprs =
        :lists.droplast(exprs)
        |> Enum.map(fn(expr) ->
          expr = typecheck(ctx, expr)
          expr_type = Millc.AST.meta(expr)[:type]
          expected_type = %TupleType{element_types: []}
          if !subtype?(ctx[:decl_types], expr_type, expected_type) do
            raise TypeError, "all but the last expressions in a block must be of type '#{format(expected_type)}'"
          end
          expr
        end)

      last_expr = typecheck(ctx, List.last(exprs))
      last_expr_type = Millc.AST.meta(last_expr)[:type]

      exprs = init_exprs ++ [last_expr]
      meta = Dict.put(meta, :type, last_expr_type)
      {:block_expr, exprs, meta}
    end
  end

  defp typecheck(ctx, {:name_expr, name, meta}) do
    type = case meta[:symbol] do
      %ModuleSymbol{} ->
        raise "this is a module, which has no type"

      %MemberSymbol{module_name: module_name, name: name} ->
        ctx[:member_types][{module_name, name}]

      %ParamSymbol{index: index} ->
        Enum.at(ctx[:param_types], index)

      %BuiltinSymbol{} ->
        raise "you're fucked"
    end
    meta = Dict.put(meta, :type, type)
    {:name_expr, name, meta}
  end

  defp typecheck(ctx, {:call_expr, callee, args, meta}) do
    callee = typecheck(ctx, callee)
    callee_type = Millc.AST.meta(callee)[:type]
    case callee_type do
      %SubType{} -> :ok
      _          -> raise TypeError, "callee is not of a subroutine type"
    end

    args =
      args
      |> Enum.with_index()
      |> Enum.map(fn({arg, i}) ->
        arg = typecheck(ctx, arg)
        expected_type = Enum.at(callee_type[:param_types], i)
        actual_type = Millc.AST.meta(arg)[:type]
        if !subtype?(ctx[:decl_types], actual_type, expected_type) do
          raise TypeError, "expected '#{expected_type}' but got '#{actual_type}'"
        end
        arg
      end)

    meta = Dict.put(meta, :type, callee_type[:return_type])

    {:call_expr, callee, args, meta}
  end

  defp typecheck(ctx, {:name_expr, name, meta}) do
    {:name_expr, name, meta}
  end

  defp typecheck(ctx, {:string_literal_expr, value, meta}) do
    {:string_literal_expr, value, Dict.put(meta, :type, %StringType{})}
  end

  defp register_decl_types(ctx, module_name, {:module, decls, _meta}) do
    List.foldl(decls, ctx, fn(decl, ctx) ->
      register_decl_types(ctx, module_name, decl)
    end)
  end

  defp register_decl_types(ctx, _module_name, {:import_decl, _, _meta}) do
    ctx
  end

  defp register_decl_types(ctx, module_name, {:struct_decl, name, fields, _meta}) do
    typed_fields = Enum.map(fields, fn({field_name, field_type_expr}) ->
      {field_name, type_expr_to_type(field_type_expr)}
    end)
    decl_type = %StructDeclType{:name => name, :fields => typed_fields}
    put_in(ctx, [:decl_types, {module_name, name}], decl_type)
  end

  defp register_decl_types(ctx, module_name, {:union_decl, name, constructors, _meta}) do
    decl_type = %UnionDeclType{:name => name, :constructors => constructors}
    put_in(ctx, [:decl_types, {module_name, name}], decl_type)
  end

  defp register_decl_types(ctx, module_name, {:alias_decl, name, aliases, _meta}) do
    aliases_type = type_expr_to_type(aliases)
    decl_type = %AliasDeclType{:name => name, :aliases => aliases_type}
    put_in(ctx, [:decl_types, {module_name, name}], decl_type)
  end

  defp register_decl_types(ctx, _module_name, {:sub_decl, _, _, _, _, _meta}) do
    ctx
  end

  defp register_member_types(ctx, module_name, {:module, decls, _meta}) do
    List.foldl(decls, ctx, fn(decl, ctx) ->
      register_member_types(ctx, module_name, decl)
    end)
  end

  defp register_member_types(ctx, _module_name, {:import_decl, _, _meta}) do
    ctx
  end

  defp register_member_types(ctx, _module_name, {:struct_decl, _, _, _meta}) do
    ctx
  end

  defp register_member_types(ctx, _module_name, {:union_decl, _, _, _meta}) do
    ctx
  end

  defp register_member_types(ctx, _module_name, {:alias_decl, _, _, _meta}) do
    ctx
  end

  defp register_member_types(ctx, module_name, {:sub_decl, name, params, return_type_expr, _body, _meta}) do
    param_types = Enum.map(params, fn({_param_name, param_type_expr}) ->
      type_expr_to_type(param_type_expr)
    end)
    return_type = type_expr_to_type(return_type_expr)
    type = %SubType{param_types: param_types, return_type: return_type}
    put_in(ctx, [:member_types, {module_name, name}], type)
  end

  defp type_expr_to_type({:name_type_expr, _name, meta}) do
    case meta[:symbol] do
      %ModuleSymbol{} ->
        raise TypeError, "expected a type name but got a module name"
      %MemberSymbol{:module_name => module_name, :name => name} ->
        %NamedType{:name => {module_name, name}}
      %ParamSymbol{} ->
        raise TypeError, "expected a type name but got a parameter name"
      %BuiltinSymbol{:name => name} ->
        case name do
          "__Top" -> %TopType{}
          "__Bottom" -> %BottomType{}
          "__String" -> %StringType{}
        end
    end
  end

  defp type_expr_to_type({:sub_type_expr, param_type_exprs, return_type_expr, _meta}) do
    param_types = Enum.map(param_type_exprs, &type_expr_to_type(&1))
    return_type = type_expr_to_type(return_type_expr)
    %SubType{:param_types => param_types, :return_type => return_type}
  end

  defp type_expr_to_type({:tuple_type_expr, element_type_exprs, _meta}) do
    element_types = Enum.map(element_type_exprs, &type_expr_to_type(&1))
    %TupleType{:element_types => element_types}
  end

  def format(%TopType{}), do: "mill.type.Top"
  def format(%BottomType{}), do: "mill.type.Bottom"
  def format(%StringType{}), do: "mill.text.String"

  def format(%TupleType{element_types: element_types}) do
    "(#{element_types |> Enum.map(&format(&1)) |> Enum.join(", ")})"
  end

  def format(%SubType{param_types: param_types, return_type: return_type}) do
    "(#{param_types |> Enum.map(&format(&1)) |> Enum.join(", ")}) => #{format(return_type)}"
  end

  def format(%NamedType{name: {module_name, name}}) do
    "#{module_name |> Enum.join(".")}.#{name}"
  end

  def descriptor(%TupleType{:element_types => element_types}) do
    "T#{element_types |> Enum.map(&descriptor(&1)) |> Enum.join("")};"
  end
end
