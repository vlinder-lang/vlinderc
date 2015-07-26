defmodule Millc.Opt.TCO do
  @behaviour Millc.Opt

  def optimize(cfg) do
    cfg |> Dict.to_list |> List.foldl(%{}, fn({id, block}, acc) ->
      Dict.put(acc, id, optimize_block(block))
    end)
  end

  defp optimize_block([{call_id, {:call, callee, args}}, {ret_id, {:ret, call_id}}]) do
    [{ret_id, {:tailcall, callee, args}}]
  end

  defp optimize_block([hd | tl]) do
    [hd | optimize_block(tl)]
  end
end
