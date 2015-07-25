defmodule Millc.LexTest do
  use ExUnit.Case

  test "Hello, world!" do
    code = """
      import mill.log

      sub main(console: log.Logger): () {
        log.info(console, "Hello, world!")
      }
    """
    {:ok, tokens} = Millc.Lex.lex(code)
    assert(tokens === [
      {:import, 1},
      {:identifier, 1, 'mill'},
      {:dot, 1},
      {:identifier, 1, 'log'},
      {:sub, 3},
      {:identifier, 3, 'main'},
      {:lparen, 3},
      {:identifier, 3, 'console'},
      {:colon, 3},
      {:identifier, 3, 'log'},
      {:dot, 3},
      {:identifier, 3, 'Logger'},
      {:rparen, 3},
      {:colon, 3},
      {:lparen, 3},
      {:rparen, 3},
      {:lbrace, 3},
      {:identifier, 4, 'log'},
      {:dot, 4},
      {:identifier, 4, 'info'},
      {:lparen, 4},
      {:identifier, 4, 'console'},
      {:comma, 4},
      {:string_literal, 4, 'Hello, world!'},
      {:rparen, 4},
      {:rbrace, 5},
    ])
  end
end
