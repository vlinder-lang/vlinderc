defmodule Millc.ParseTest do
  use ExUnit.Case

  test "Hello, world!" do
    code = """
      import mill.log

      sub main(console: log.Logger): () {
        log.info(console, "Hello, world!")
      }
    """
    {:ok, tokens} = Millc.Lex.lex(code)
    {:ok, module} = Millc.Parse.parse(tokens)
    assert(module === {
      :module,
      [
        {:import_decl, ['mill', 'log'], %{}},
        {:sub_decl,
          'main',
          [{'console', {:name_type_expr, {:qualified_name, 'log', 'Logger'}, %{}}}],
          {:tuple_type_expr, [], %{}},
          {:block_expr,
            [
              {:call_expr,
                {:name_expr, {:qualified_name, 'log', 'info'}, %{}},
                [
                  {:name_expr, {:unqualified_name, 'console'}, %{}},
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
