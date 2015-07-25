defmodule Millc.Lex do
  def lex(text) do
    case (text |> to_char_list() |> :millc_lex.string()) do
      {:ok, tokens, _end_line} -> {:ok, tokens}
      {_, _, _} -> :error
    end
  end
end
