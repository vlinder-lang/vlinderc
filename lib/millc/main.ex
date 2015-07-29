defmodule Millc.Main do
  def main([output_dir | inputs]) do
    parse_modules(inputs)
    |> resolve_names()
    |> typecheck()
    |> assemble()
    |> Enum.each(fn({name, data}) ->
      path = Path.join(output_dir, Enum.join(name, ".") <> ".millo")
      File.write!(path, data)
    end)
  end

  defp parse_modules(args) do
    args
    |> Enum.map(fn(full_name) ->
      [dir, name] = String.split(full_name, ":", parts: 2)
      path = Path.join([dir | String.split(name, ".")]) <> ".mill"
      code = File.read!(path)
      {:ok, tokens} = Millc.Lex.lex(code)
      {:ok, module} = Millc.Parse.parse(tokens)
      {String.split(name, "."), module}
    end)
    |> List.foldl(%{}, fn({name, module}, acc) -> Dict.put(acc, name, module) end)
  end

  defp resolve_names(modules) do
    modules
    |> Dict.keys()
    |> List.foldl(%{}, fn(module_name, acc) ->
      {:ok, module} = Millc.Name.resolve_module(module_name, modules)
      Dict.put(acc, module_name, module)
    end)
  end

  defp typecheck(modules) do
    {:ok, modules} = Millc.Type.typecheck(modules)
    modules
  end

  defp assemble(modules) do
    modules
    |> Dict.to_list()
    |> Enum.map(fn({name, module}) ->
      {name, Millc.Assembly.assemble(name, module)}
    end)
  end
end
