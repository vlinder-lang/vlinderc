defmodule Millc.SSA do
  def outgoing(block) do
    List.foldl(block, HashSet.new, fn({_instr_id, instr}, target_ids) ->
      case instr do
        {:goto, target_id} ->
          Set.put(target_ids, target_id)

        {:if, _condition, then_id, else_id} ->
          target_ids |> Set.put(then_id) |> Set.put(else_id)

        _ ->
          target_ids
      end
    end)
  end

  def replace_instr_uses(cfg, of, to) do
    replace = &(if &1 === of, do: to, else: &1)

    cfg |> Dict.to_list |> List.foldl(%{}, fn({block_id, block}, acc) ->
      block = Enum.map(block, fn({instr_id, instr}) ->
        instr = case instr do
          {:call, callee, args} ->
            {:call, replace.(callee), Enum.map(args, replace)}

          {:goto, block_id} ->
            {:goto, block_id}

          {:if, condition, then_id, else_id} ->
            {:if, replace.(condition), then_id, else_id}

          {:ldarg, index} ->
            {:ldarg, index}

          {:ldint, value} ->
            {:ldint, value}

          {:ldgbl, module_name, name} ->
            {:ldgbl, module_name, name}

          {:ldstr, value} ->
            {:ldstr, value}

          {:new, type} ->
            {:new, type}

          {:ret, value} ->
            {:ret, replace.(value)}

          {:tailcall, callee, args} ->
            {:tailcall, replace.(callee), Enum.map(args, replace)}
        end
        {instr_id, instr}
      end)
      Dict.put(acc, block_id, block)
    end)
  end

  def dump(graph) do
    {:ok, builder} = Agent.start_link(fn() -> "" end)
    try do
      build = fn(str) ->
        :ok = Agent.update(builder, fn(txt) -> txt <> str end)
      end

      build.("digraph {\n")

      Enum.each(graph, fn({block_id, block}) ->
        dump_block(build, block_id, block)
      end)

      Enum.each(graph, fn({block_id, block}) ->
        Enum.each(outgoing(block), fn(other_id) ->
          build.("#{block_id} -> #{other_id}\n")
        end)
      end)

      build.("}\n")

      File.write(
        "/Users/rightfold/Desktop/graph.dot",
        Agent.get(builder, fn(txt) -> txt end),
        [:write]
      )
    after
      :ok = Agent.stop(builder)
    end
  end

  defp dump_block(build, block_id, block) do
    build.("""
      #{block_id} [
        shape = none
        label = <<table border="0" cellborder="1" cellspacing="0">
          <tr>
            <td colspan="2">#{block_id}</td>
          </tr>
    """)
    Enum.each(block, fn({instr_id, instr}) ->
      build.("|")
      dump_instr(build, instr_id, instr)
    end)
    build.("""
        </table>>
      ]\n
    """)
  end

  defp dump_instr(build, instr_id, instr) do
    build.("""
      <tr>
        <td align="right">#{instr_id}</td>
        <td align="left">
    """)

    case instr do
      {:call, callee, args} ->
        build.("call #{callee} #{Enum.join(args, " ")}")

      {:goto, block_id} ->
        build.("goto #{block_id}")

      {:if, condition, then_id, else_id} ->
        build.("if #{condition} #{then_id} #{else_id}")

      {:ldarg, index} ->
        build.("ldarg #{index}")

      {:ldint, value} ->
        build.("ldint #{value}")

      {:ldgbl, module_name, name} ->
        build.("ldgbl #{Enum.join(module_name, ".")} #{name}")

      {:ldstr, value} ->
        build.("ldstr #{inspect(value)}")

      {:new, type} ->
        build.("new #{Millc.Type.descriptor(type)}")

      {:ret, value} ->
        build.("ret #{value}")

      {:tailcall, callee, args} ->
        build.("tailcall #{callee} #{Enum.join(args, " ")}")
    end

    build.("""
        </td>
      </tr>\n
    """)
  end
end
