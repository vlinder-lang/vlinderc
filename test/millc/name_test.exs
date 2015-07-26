defmodule Millc.NameTest do
  alias Millc.Name.MemberSymbol, as: MemberSymbol
  alias Millc.Name.LocalSymbol, as: LocalSymbol

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
      union Level {
        Debug
        Info
        Warning
        Error
        Critical
      }

      struct Record {
        level: Level
        message: String
      }

      alias Logger = (Record) => ()

      sub info(logger: Logger, message: String): () { }
    """
    {:ok, mill_log_tokens} = Millc.Lex.lex(mill_log_code)
    {:ok, mill_log} = Millc.Parse.parse(mill_log_tokens)

    {:ok, module} = Millc.Name.resolve_module(['main'], %{
      ['main'] => main,
      ['mill', 'log'] => mill_log,
    })
    assert(module === {
      :module,
      [
        {:import_decl, ['mill', 'log'], %{}},
        {:sub_decl,
          'main',
          [
            {
              'console',
              {:name_type_expr,
                {:qualified_name, 'log', 'Logger'},
                %{:symbol => %MemberSymbol{:module_name => ['mill', 'log'],
                                           :name => 'Logger'}},
              },
            },
          ],
          {:tuple_type_expr, [], %{}},
          {:block_expr,
            [
              {:call_expr,
                {:name_expr,
                  {:qualified_name, 'log', 'info'},
                  %{:symbol => %MemberSymbol{:module_name => ['mill', 'log'],
                                             :name => 'info'}}
                },
                [
                  {:name_expr,
                    {:unqualified_name, 'console'},
                    %{:symbol => %LocalSymbol{:name => 'console'}},
                  },
                  {:string_literal_expr, 'Hello, world!', %{}},
                ],
                %{},
              },
            ],
            %{},
          },
          %{},
        },
      ],
      %{},
    })

  end
end
