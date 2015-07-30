defmodule Millc.Opt.CIE do
  @behaviour Millc.Opt

  def optimize(cfg) do
    {cfg, _cache, to_replace} =
      cfg
      |> Dict.to_list
      |> List.foldl({%{}, %{}, %{}}, fn({id, block}, {cfg, cache, to_replace}) ->
        {block, cache, to_replace} = optimize_block(block, cache, to_replace)
        {Dict.put(cfg, id, block), cache, to_replace}
      end)
    to_replace |> Dict.to_list |> List.foldl(cfg, fn({of, to}, cfg) ->
      Millc.SSA.replace_instr_uses(cfg, of, to)
    end)
  end

  def optimize_block(block, cache, to_replace) do
    List.foldl(
      block,
      {[], cache, to_replace},
      fn({instr_id, instr}, {block, cache, to_replace}) ->
        yes = fn() ->
          if cache[instr] === nil do
            {
              block ++ [{instr_id, instr}],
              Dict.put(cache, instr, instr_id),
              to_replace,
            }
          else
            {block, cache, Dict.put(to_replace, instr_id, cache[instr])}
          end
        end

        case instr do
          {:ldarg, _index} -> yes.()
          {:ldgbl, _module_name, _name} -> yes.()
          {:ldstr, _value} -> yes.()
          {:new, _type, _fields} -> yes.()

          _ -> {block ++ [{instr_id, instr}], cache, to_replace}
        end
      end
    )
  end
end
