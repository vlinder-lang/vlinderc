defmodule Millc.Assembly do
  @moduledoc """
  `Millc.Assembly` assembles Mill module files.
  """

  def assemble(module_name, module = {:module, decls, _meta}) do
    result = %{
      name: Enum.join(module_name, "."),
      imports: imports(module),
      unions: unions(module),
      structs: structs(module),
      aliases: aliases(module),
      subs: subs(module),
      foreignSubs: foreign_subs(module),
    }
    JSON.encode!(result)
  end

  defp imports({:module, decls, _meta}) do
    Enum.flat_map(decls, fn(decl) ->
      case decl do
        {:import_decl, module_name, _meta} ->
          [Enum.join(module_name, ".")]
        _ -> []
      end
    end)
  end

  defp unions(_module) do
    []
  end

  defp structs(_module) do
    []
  end

  defp aliases(_module) do
    []
  end

  defp subs({:module, decls, _meta}) do
    Enum.flat_map(decls, fn(decl) ->
      case decl do
        {:sub_decl, name, params, return_type_expr, body, meta} ->
          parameters = Enum.map(params, fn({param_name, param_type_expr}) ->
            param_type = Millc.Type.type_expr_to_type(param_type_expr)
            %{name: param_name, type: Millc.Type.descriptor(param_type)}
          end)
          return_type = Millc.Type.type_expr_to_type(return_type_expr)
          {local_count, body} =
            decl
            |> Millc.AST2SSA.codegen()
            |> Millc.SSA2Bytecode.codegen()
          sub = %{
            name: name,
            parameters: parameters,
            returnType: Millc.Type.descriptor(return_type),
            localCount: local_count,
            body: body,
          }
          [sub]

        _ ->
          []
      end
    end)
  end

  defp foreign_subs(_module) do
    []
  end
end
