defmodule Millc.TypeTest do
  import Millc.Type
  alias Millc.Type.StringType, as: StringType
  alias Millc.Type.TupleType, as: TupleType
  alias Millc.Type.SubroutineType, as: SubroutineType
  alias Millc.Type.NamedType, as: NamedType

  use ExUnit.Case

  test "same_type?" do
    assert(same_type?(%StringType{}, %StringType{}))
    assert(same_type?(%TupleType{:element_types => []},
                      %TupleType{:element_types => []}))
    assert(same_type?(%SubroutineType{:param_types => [], :return_type => %StringType{}},
                      %SubroutineType{:param_types => [], :return_type => %StringType{}}))
    assert(same_type?(%NamedType{:name => ['mill', 'log', 'Logger']},
                      %NamedType{:name => ['mill', 'log', 'Logger']}))

    assert(!same_type?(%StringType{}, %TupleType{:element_types => []}))
    assert(!same_type?(%TupleType{:element_types => []},
                      %SubroutineType{:param_types => [], :return_type => %StringType{}}))
    assert(!same_type?(%SubroutineType{:param_types => [], :return_type => %StringType{}},
                      %NamedType{:name => ['mill', 'log', 'Logger']}))
    assert(!same_type?(%NamedType{:name => ['mill', 'log', 'Logger']},
                      %StringType{}))
  end
end
