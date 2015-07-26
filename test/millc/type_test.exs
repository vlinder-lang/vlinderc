defmodule Millc.TypeTest do
  import Millc.Type
  alias Millc.Type.StringType, as: StringType
  alias Millc.Type.TupleType, as: TupleType
  alias Millc.Type.SubType, as: SubType
  alias Millc.Type.NamedType, as: NamedType

  alias Millc.Type.UnionDeclType, as: UnionDeclType
  alias Millc.Type.StructDeclType, as: StructDeclType
  alias Millc.Type.AliasDeclType, as: AliasDeclType

  use ExUnit.Case

  test "same_type?" do
    decl_types = %{
      {["mill", "log"], "Level"} => %UnionDeclType{
        :name => {["mill", "log"], "Level"},
        :constructors => [
          {"Debug", []},
          {"Info", []},
          {"Warning", []},
          {"Error", []},
          {"Critical", []},
        ],
      },
      {["mill", "log"], "Record"} => %StructDeclType{
        :name => {["mill", "log"], "Record"},
        :fields => [
          {"level", %NamedType{:name => {["mill", "log"], "Level"}}},
          {"message", %StringType{}},
        ],
      },
      {["mill", "log"], "Logger"} => %AliasDeclType{
        :name => {["mill", "log"], "Logger"},
        :aliases => %SubType{
          :param_types => [%NamedType{:name => {["mill", "log"], "Record"}}],
          :return_type => %TupleType{:element_types => []},
        },
      },
    }

    [
      {
        %NamedType{:name => {["mill", "log"], "Level"}},
        %NamedType{:name => {["mill", "log"], "Level"}},
      },
      {
        %NamedType{:name => {["mill", "log"], "Record"}},
        %NamedType{:name => {["mill", "log"], "Record"}},
      },
      {
        %NamedType{:name => {["mill", "log"], "Logger"}},
        %NamedType{:name => {["mill", "log"], "Logger"}},
      },
      {
        %NamedType{:name => {["mill", "log"], "Logger"}},
        %SubType{
          :param_types => [%NamedType{:name => {["mill", "log"], "Record"}}],
          :return_type => %TupleType{:element_types => []},
        },
      },
    ]
    |> Enum.each(fn({a, b}) ->
      assert(same_type?(decl_types, a, b))
    end)
  end

  test "type_check" do
    mill_text_code = """
      alias String = __String
    """
    {:ok, mill_text_tokens} = Millc.Lex.lex(mill_text_code)
    {:ok, mill_text} = Millc.Parse.parse(mill_text_tokens)

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

    modules = %{
      ["mill", "text"] => mill_text,
      ["mill", "log"] => mill_log,
    }

    {:ok, mill_text} = Millc.Name.resolve_module(["mill", "text"], modules)
    {:ok, mill_log} = Millc.Name.resolve_module(["mill", "log"], modules)

    modules = %{
      ["mill", "text"] => mill_text,
      ["mill", "log"] => mill_log,
    }

    {:ok, result} = type_check(modules)
    IO.inspect(result)
  end
end
