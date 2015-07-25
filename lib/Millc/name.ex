defmodule Millc.Name do
  defmodule Context do
    @derive [Access]
    defstruct symbol_table: nil, modules: nil
  end

  def resolve_module(module, modules) do
    ctx = %Context{
      :symbol_table => %{},
      :modules => modules,
    }
    {_, module} = resolve(ctx, modules[module])
    {:ok, module}
  end

  defp resolve(ctx, {:module, decls, meta}) do
    {ctx, decls} = List.foldl(decls, {ctx, []}, fn(decl, {ctx, decls}) ->
      {ctx, decl} = resolve(ctx, decl)
      {ctx, decls ++ [decl]}
    end)
    {ctx, {:module, decls, meta}}
  end

  defp resolve(ctx, {:import_decl, module_name, meta}) do
    # TODO: Check if module is known, and raise if it isn't.
    symbol = {:module_symbol, module_name}
    ctx = put_in(ctx, [:symbol_table, List.last(module_name)], symbol)
    {ctx, {:import_decl, module_name, meta}}
  end

  defp resolve(ctx, {:sub_decl, name, params, return_type, body, meta}) do
    symbol = {:global_symbol, name}
    ctx = put_in(ctx, [:symbol_table, name], symbol)

    params = List.foldl(params, [], fn({param_name, type}, params) ->
      params ++ [{param_name, resolve(ctx, type)}]
    end)

    body_ctx = List.foldl(params, ctx, fn({param_name, _type}, ctx) ->
      param_symbol = {:local_symbol, param_name}
      put_in(ctx, [:symbol_table, param_name], param_symbol)
    end)

    return_type = resolve(ctx, return_type)

    body = resolve(body_ctx, body)

    {ctx, {:sub_decl, name, params, return_type, body, meta}}
  end

  defp resolve(ctx, {:call_expr, callee, args, meta}) do
    callee = resolve(ctx, callee)
    args = Enum.map(args, &resolve(ctx, &1))
    {:call_expr, callee, args, meta}
  end

  defp resolve(ctx, {:name_expr, name, meta}) do
    symbol = lookup_name(ctx, name)
    meta = Dict.put(meta, :symbol, symbol)
    {:name_expr, name, meta}
  end

  defp resolve(_ctx, expr = {:string_literal_expr, _, _}) do
    expr
  end

  defp resolve(ctx, {:block_expr, exprs, meta}) do
    exprs = Enum.map(exprs, &resolve(ctx, &1))
    {:block_expr, exprs, meta}
  end

  defp resolve(ctx, {:name_type_expr, name, meta}) do
    symbol = lookup_name(ctx, name)
    meta = Dict.put(meta, :symbol, symbol)
    {:name_type_expr, name, meta}
  end

  defp resolve(ctx, {:tuple_type_expr, element_types, meta}) do
    element_types = Enum.map(element_types, &resolve(ctx, &1))
    {:tuple_type_expr, element_types, meta}
  end

  defp lookup_name(ctx, {:unqualified_name, name}) do
    case ctx[:symbol_table][name] do
      nil -> raise "name '#{name}' not in scope"
      symbol -> symbol
    end
  end

  defp lookup_name(ctx, {:qualified_name, module_name_last, name}) do
    case ctx[:symbol_table][module_name_last] do
      nil -> raise "name '#{module_name_last}' not in scope"
      {:module_symbol, module_name} ->
        module = ctx[:modules][module_name]
        exported_names = exported_names(module)
        if Set.member?(exported_names, name) do
          {:member_symbol, module_name, name}
        else
          raise "name '#{name}' is not exported by module '#{module_name_last}'"
        end
      _ -> raise "name '#{module_name_last}' does not refer to an imported module"
    end
  end

  defp exported_names({:module, decls, _meta}) do
    List.foldl(decls, HashSet.new, fn(decl, names) ->
      case decl do
        {:import_decl, _module_name, _meta} ->
          HashSet.empty

        {:sub_decl, name, _params, _return_type, _body, _meta} ->
          Set.put(names, name)
      end
    end)
  end
end
