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
    assert(main_module === {
      :module,
      [
        {:import_decl, ["mill", "log"], %{}},
        {:sub_decl,
          "main",
          [
            {
              "console",
              {:name_type_expr,
                {:qualified_name, "log", "Logger"},
                %{:symbol => %MemberSymbol{:module_name => ["mill", "log"],
                                           :name => "Logger"}},
              },
            },
          ],
          {:tuple_type_expr, [], %{}},
          {:block_expr,
            [
              {:call_expr,
                {:name_expr,
                  {:qualified_name, "log", "info"},
                  %{:symbol => %MemberSymbol{:module_name => ["mill", "log"],
                                             :name => "info"}}
                },
                [
                  {:name_expr,
                    {:unqualified_name, "console"},
                    %{:symbol => %LocalSymbol{:name => "console"}},
                  },
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

    {:ok, mill_log_module} = Millc.Name.resolve_module(["mill", "log"], modules)
    assert(mill_log_module === {
      :module,
      [
        {:import_decl, ["mill", "text"], %{}},
        {:union_decl,
          "Level",
          [
            {"Debug", []},
            {"Info", []},
            {"Warning", []},
            {"Error", []},
            {"Critical", []}
          ],
          %{},
        },
        {:struct_decl,
          "Record",
          [
            {
              "level",
              {:name_type_expr,
                {:unqualified_name, "Level"},
                %{
                  :symbol => %MemberSymbol{
                    :module_name => ["mill", "log"],
                    :name => "Level",
                  },
                },
              },
            },
            {
              "message",
              {:name_type_expr,
                {:qualified_name, "text", "String"},
                %{
                  :symbol => %MemberSymbol{
                    :module_name => ["mill", "text"],
                    :name => "String",
                  },
                },
              }
            },
          ],
          %{},
        },
        {:alias_decl,
          "Logger",
          {:sub_type_expr,
            [
              {:name_type_expr,
                {:unqualified_name, "Record"},
                %{
                  :symbol => %MemberSymbol{
                    :module_name => ["mill", "log"],
                    :name => "Record",
                  },
                },
              },
            ],
            {:tuple_type_expr, [], %{}},
            %{},
          },
          %{},
        },
        {:sub_decl,
          "info",
          [
            {
              "logger",
              {:name_type_expr,
                {:unqualified_name, "Logger"},
                %{
                  :symbol => %MemberSymbol{
                    :module_name => ["mill", "log"],
                    :name => "Logger",
                  },
                },
              },
            },
            {
              "message",
              {:name_type_expr,
                {:qualified_name, "text", "String"},
                %{
                  :symbol => %MemberSymbol{
                    :module_name => ["mill", "text"],
                    :name => "String",
                  },
                },
              },
            },
          ],
          {:tuple_type_expr, [], %{}},
          {:block_expr, [], %{}},
          %{},
        },
      ],
      %{},
    })
  end
end
