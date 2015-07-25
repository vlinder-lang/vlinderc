defmodule Millc.NameTest do
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
      sub Logger(): () { }
      sub info(): () { }
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
                %{:symbol => {:member_symbol, ['mill', 'log'], 'Logger'}},
              },
            },
          ],
          {:tuple_type_expr, [], %{}},
          {:block_expr,
            [
              {:call_expr,
                {:name_expr,
                  {:qualified_name, 'log', 'info'},
                  %{:symbol => {:member_symbol, ['mill', 'log'], 'info'}},
                },
                [
                  {:name_expr,
                    {:unqualified_name, 'console'},
                    %{:symbol => {:local_symbol, 'console'}},
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
