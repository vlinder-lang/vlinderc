defmodule Millc.AST2SSA do
  defmodule CFGBuilder do
    def start_link() do
      Agent.start_link(fn() -> {%{}, nil, 1} end)
    end

    def stop(builder) do
      Agent.stop(builder)
    end

    def get(builder) do
      Agent.get(builder, fn({cfg, _current_block, _id}) -> cfg end)
    end

    defp id(builder, prefix) do
      Agent.get_and_update(builder, fn({cfg, current_block, id}) ->
        {"#{prefix}#{id}", {cfg, current_block, id + 1}}
      end)
    end

    def block(builder) do
      block_id = id(builder, "b")
      Agent.update(builder, fn({cfg, _current_block, id}) ->
        {Dict.put(cfg, block_id, []), block_id, id}
      end)
      block_id
    end

    def instr(builder, instr) do
      instr_id = id(builder, "i")
      Agent.update(builder, fn({cfg, current_block, id}) ->
        {
          update_in(cfg, [current_block], &(&1 ++ [{instr_id, instr}])),
          current_block,
          id,
        }
      end)
      instr_id
    end
  end

  def codegen({:sub_decl, _name, _params, _return_type, body, _meta}) do
    {:ok, builder} = CFGBuilder.start_link()
    try do
      entry = CFGBuilder.block(builder)
      result = codegen(builder, body)
      CFGBuilder.instr(builder, {:ret, result})
      CFGBuilder.get(builder)
    after
      CFGBuilder.stop(builder)
    end
  end

  defp codegen(builder, {:block_expr, exprs, _meta}) do
    if exprs === [] do
      CFGBuilder.instr(builder, {:new, %Millc.Type.TupleType{:element_types => []}, []})
    else
      exprs
      |> :lists.droplast()
      |> Enum.each(&codegen(builder, &1))
      codegen(builder, List.last(exprs))
    end
  end

  defp codegen(builder, {:call_expr, callee, args, _meta}) do
    callee_id = codegen(builder, callee)
    arg_ids = Enum.map(args, &codegen(builder, &1))
    CFGBuilder.instr(builder, {:call, callee_id, arg_ids})
  end

  defp codegen(builder, {:name_expr, _name, meta}) do
    instr = case meta[:symbol] do
      %Millc.Name.MemberSymbol{:module_name => module_name, :name => name} ->
        {:ldgbl, module_name, name}

      %Millc.Name.ParamSymbol{:index => index} ->
        {:ldarg, index}
    end
    CFGBuilder.instr(builder, instr)
  end

  defp codegen(builder, {:string_literal_expr, value, _meta}) do
    CFGBuilder.instr(builder, {:ldstr, value})
  end

  defp codegen(builder, {:struct_literal_expr, type_expr, fields, _meta}) do
    type = Millc.Type.type_expr_to_type(type_expr)
    field_ids = Enum.map(fields, fn({name, value}) ->
      field_id = codegen(builder, value)
      {name, field_id}
    end)
    CFGBuilder.instr(builder, {:new, type, field_ids})
  end
end
