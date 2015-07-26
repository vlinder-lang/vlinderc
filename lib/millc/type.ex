defmodule Millc.Type do
  defmodule StringType do
    defstruct []
  end

  defmodule TupleType do
    @derive [Access]
    defstruct element_types: nil
  end

  defmodule SubroutineType do
    @derive [Access]
    defstruct param_types: nil, return_type: nil
  end

  defmodule NamedType do
    @derive [Access]
    defstruct name: nil
  end

  def subtype?(a, b) do
    same_type?(a, b)
  end

  def supertype?(a, b) do
    subtype?(b, a)
  end

  def same_type?(a, b) do
    a === b
  end
end
