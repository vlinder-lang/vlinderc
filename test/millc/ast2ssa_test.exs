defmodule Millc.AST2SSATest do
  use ExUnit.Case

  test "dump" do
    main_code = """
      import mill.log

      sub main(console: log.Logger): () {
        log.info(console, "Hello, world!")
        log.info(console, "Hello, world!")
        log.info(console, "Hello, world!")
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

    {:ok, main_module} = Millc.Name.resolve_module(["main"], modules)

    main_decl = (
      {:module, [_, main_decl], _meta} = main_module
      main_decl
    )

    graph = Millc.AST2SSA.codegen(main_decl)
    IO.inspect(graph)
    graph = Millc.Opt.TCO.optimize(graph)
    IO.inspect(graph)
    :ok = Millc.SSA.dump(graph)
  end
end
