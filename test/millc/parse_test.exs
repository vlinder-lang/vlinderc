defmodule Millc.ParseTest do
  use ExUnit.Case

  test "Hello, world!" do
    code = """
      import mill.log

      sub main(console: log.Logger): () {
        mk T{}
        mk U{x: "a"}
        mk V{x: "a", y: "b"}
        log.info(console, "Hello, world!")
      }
    """
    {:ok, tokens} = Millc.Lex.lex(code)
    {:ok, module} = Millc.Parse.parse(tokens)
    assert(module === {
      :module,
      [
        {:import_decl, ["mill", "log"], %{}},
        {:sub_decl,
          "main",
          [{"console", {:name_type_expr, {:qualified_name, "log", "Logger"}, %{}}}],
          {:tuple_type_expr, [], %{}},
          {:block_expr,
            [
              {:struct_literal_expr,
                {:name_type_expr, {:unqualified_name, "T"}, %{}},
                [],
                %{},
              },
              {:struct_literal_expr,
                {:name_type_expr, {:unqualified_name, "U"}, %{}},
                [{"x", {:string_literal_expr, "a", %{}}}],
                %{},
              },
              {:struct_literal_expr,
                {:name_type_expr, {:unqualified_name, "V"}, %{}},
                [
                  {"x", {:string_literal_expr, "a", %{}}},
                  {"y", {:string_literal_expr, "b", %{}}},
                ],
                %{},
              },
              {:call_expr,
                {:name_expr, {:qualified_name, "log", "info"}, %{}},
                [
                  {:name_expr, {:unqualified_name, "console"}, %{}},
                  {:string_literal_expr, "Hello, world!", %{}},
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
