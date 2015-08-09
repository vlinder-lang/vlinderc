defmodule Millc.TypeTest.Helpers do
  defmacro ok(name) do
    quote do
      test unquote(name) do
        modules = module(unquote(name))
        {:ok, _result} = Millc.Type.typecheck(modules)
      end
    end
  end

  defmacro error(name, expected_error) do
    quote do
      test unquote(name) do
        modules = module(unquote(name))
        {:error, error} = Millc.Type.typecheck(modules)
        assert(error === unquote(expected_error))
      end
    end
  end
end

defmodule Millc.TypeTest do
  use ExUnit.Case
  import Millc.TypeTest.Helpers

  alias Millc.Type.StringType, as: StringType
  alias Millc.Type.TupleType, as: TupleType
  alias Millc.Type.SubType, as: SubType
  alias Millc.Type.NamedType, as: NamedType

  alias Millc.Type.UnionDeclType, as: UnionDeclType
  alias Millc.Type.StructDeclType, as: StructDeclType
  alias Millc.Type.AliasDeclType, as: AliasDeclType

  alias Millc.Type.TypeError, as: TypeError

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
      assert(Millc.Type.same_type?(decl_types, a, b))
    end)
  end

  defp module(name) do
    file = "#{__DIR__}/type_test_data/#{name}"
    {:ok, code} = File.read(file)
    {:ok, tokens} = Millc.Lex.lex(code)
    {:ok, module} = Millc.Parse.parse(tokens)
    {:ok, module} = Millc.Name.resolve_module(["m"], %{["m"] => module})
    %{["m"] => module}
  end

  ok("ok/empty.mill")
  ok("ok/text.mill")
  ok("ok/return_type.mill")
  ok("ok/kitchen_sink.mill")

  error("error/return_type.mill", %TypeError{message: "expected '()' but got 'mill.text.String'"})
  error("error/non_unit.mill", %TypeError{message: "all but the last expressions in a block must be of type '()'"})
end
