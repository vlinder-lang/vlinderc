defmodule Millc.AST do
  def meta(node) do
    node |> :erlang.tuple_to_list() |> List.last()
  end
end
