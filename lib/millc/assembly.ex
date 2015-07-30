defmodule Millc.Assembly do
  @moduledoc """
  `Millc.Assembly` assembles Mill module files.
  """

  def assemble(module_name, module = {:module, _decls, _meta}) do
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

  defp unions({:module, decls, _meta}) do
    Enum.flat_map(decls, fn(decl) ->
      case decl do
        {:union_decl, name, constructors, _meta} ->
          union = %{
            name: name,
            constructors: Enum.map(constructors, fn({constructor_name, []}) ->
              %{name: constructor_name, parameters: []}
            end),
          }
          [union]
        _ ->
          []
      end
    end)
  end

  defp structs({:module, decls, _meta}) do
    Enum.flat_map(decls, fn(decl) ->
      case decl do
        {:struct_decl, name, fields, _meta} ->
          struct = %{
            name: name,
            fields: Enum.map(fields, fn({field_name, field_type_expr}) ->
              field_type = Millc.Type.type_expr_to_type(field_type_expr)
              %{
                name: field_name,
                type: Millc.Type.descriptor(field_type),
              }
            end),
          }
          [struct]
        _ ->
          []
      end
    end)
  end

  defp aliases({:module, decls, _meta}) do
    Enum.flat_map(decls, fn(decl) ->
      case decl do
        {:alias_decl, name, aliases_type_expr, _meta} ->
          aliases = Millc.Type.type_expr_to_type(aliases_type_expr)
          [%{name: name, type: Millc.Type.descriptor(aliases)}]
        _ ->
          []
      end
    end)
  end

  defp subs({:module, decls, _meta}) do
    Enum.flat_map(decls, fn(decl) ->
      case decl do
        {:sub_decl, name, params, return_type_expr, _body, _meta} ->
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
