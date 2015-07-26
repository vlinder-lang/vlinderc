defmodule Millc.TypeTest do
  import Millc.Type
  alias Millc.Type.StringType, as: StringType
  alias Millc.Type.TupleType, as: TupleType
  alias Millc.Type.SubroutineType, as: SubroutineType
  alias Millc.Type.NamedType, as: NamedType

  alias Millc.Type.UnionDeclType, as: UnionDeclType
  alias Millc.Type.StructDeclType, as: StructDeclType
  alias Millc.Type.AliasDeclType, as: AliasDeclType

  use ExUnit.Case

  test "same_type?" do
    decl_types = %{
      ['mill', 'log', 'Level'] => %UnionDeclType{
        :name => ['mill', 'log', 'Level'],
        :constructors => [
          {'Debug', []},
          {'Info', []},
          {'Warning', []},
          {'Error', []},
          {'Critical', []},
        ],
      },
      ['mill', 'log', 'Record'] => %StructDeclType{
        :name => ['mill', 'log', 'Record'],
        :fields => [
          {'level', %NamedType{:name => ['mill', 'log', 'Level']}},
          {'message', %StringType{}},
        ],
      },
      ['mill', 'log', 'Logger'] => %AliasDeclType{
        :name => ['mill', 'log', 'Logger'],
        :aliases => %SubroutineType{
          :param_types => [%NamedType{:name => ['mill', 'log', 'Record']}],
          :return_type => %TupleType{:element_types => []},
        },
      },
    }

    [
      {
        %NamedType{:name => ['mill', 'log', 'Level']},
        %NamedType{:name => ['mill', 'log', 'Level']},
      },
      {
        %NamedType{:name => ['mill', 'log', 'Record']},
        %NamedType{:name => ['mill', 'log', 'Record']},
      },
      {
        %NamedType{:name => ['mill', 'log', 'Logger']},
        %NamedType{:name => ['mill', 'log', 'Logger']},
      },
      {
        %NamedType{:name => ['mill', 'log', 'Logger']},
        %SubroutineType{
          :param_types => [%NamedType{:name => ['mill', 'log', 'Record']}],
          :return_type => %TupleType{:element_types => []},
        },
      },
    ]
    |> Enum.each(fn({a, b}) ->
      assert(same_type?(decl_types, a, b))
    end)
  end
end
