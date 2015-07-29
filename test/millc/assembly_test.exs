defmodule Millc.AssemblyTest do
  use ExUnit.Case

  test "Hello, world!" do
    main_code = """
      import mill.log

      sub main(console: log.Logger): () {
        log.info(console, "Hello, world!")
      }
    """
    {:ok, main_tokens} = Millc.Lex.lex(main_code)
    {:ok, main} = Millc.Parse.parse(main_tokens)

    mill_log_code = """
      import mill.text

      union Level {
        Debug
        Info
        Warning
        Error
        Critical
      }

      struct Record {
        level: Level
        message: text.String
      }

      alias Logger = (Record) => ()

      sub info(logger: Logger, message: text.String): () { }
    """
    {:ok, mill_log_tokens} = Millc.Lex.lex(mill_log_code)
    {:ok, mill_log} = Millc.Parse.parse(mill_log_tokens)

    mill_text_code = """
      alias String = __String
    """

    {:ok, mill_text_tokens} = Millc.Lex.lex(mill_text_code)
    {:ok, mill_text} = Millc.Parse.parse(mill_text_tokens)

    modules = %{
      ["main"] => main,
      ["mill", "log"] => mill_log,
      ["mill", "text"] => mill_text,
    }

    {:ok, result} =
      modules
      |> Dict.to_list()
      |> List.foldl(%{}, fn({module_name, module}, acc) ->
        {:ok, module} = Millc.Name.resolve_module(module_name, modules)
        Dict.put(acc, module_name, module)
      end)
      |> Millc.Type.typecheck()

    IO.puts(Millc.Assembly.assemble(["main"], result[["main"]]))
  end
end
