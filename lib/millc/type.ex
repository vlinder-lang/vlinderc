defmodule Millc.Type do
  alias Millc.Name.ModuleSymbol, as: ModuleSymbol
  alias Millc.Name.MemberSymbol, as: MemberSymbol
  alias Millc.Name.ParamSymbol, as: ParamSymbol
  alias Millc.Name.BuiltinSymbol, as: BuiltinSymbol

  defmodule StringType do
    defstruct []
  end

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
    same_type?(decl_types, a, b)
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

  defmodule Context do
    @derive [Access]
    defstruct decl_types: nil
  end

  def type_check(modules) do
    ctx = %Context{
      :decl_types => %{},
    }

    ctx = Dict.to_list(modules) |> List.foldl(ctx, fn({module_name, module}, ctx) ->
      register_decl_types(ctx, module_name, module)
    end)

    {:ok, modules}
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

  defp type_expr_to_type({:name_type_expr, _name, meta}) do
    case meta[:symbol] do
      %ModuleSymbol{} ->
        raise "this is a module, not a type"
      %MemberSymbol{:module_name => module_name, :name => name} ->
        %NamedType{:name => {module_name, name}}
      %ParamSymbol{} ->
        raise "this is a parameter, not a type"
      %BuiltinSymbol{:name => name} ->
        case name do
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

  def descriptor(%TupleType{:element_types => element_types}) do
    "T#{element_types |> Enum.map(&descriptor(&1)) |> Enum.join("")};"
  end
end
