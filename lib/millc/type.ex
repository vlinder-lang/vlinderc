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

  defmodule StructDeclType do
    @derive [Access]
    defstruct name: nil, fields: nil
  end

  defmodule UnionDeclType do
    @derive [Access]
    defstruct name: nil, constructors: nil
  end

  defmodule AliasDeclType do
    @derive [Access]
    defstruct name: nil, aliases: nil
  end

  def subtype?(decl_types, a, b) do
    same_type?(decl_types, a, b)
  end

  def supertype?(decl_types, a, b) do
    subtype?(decl_types, b, a)
  end

  def same_type?(decl_types, a, b) do
    a = resolve_aliases(decl_types, a)
    b = resolve_aliases(decl_types, b)
    a === b
  end

  def resolve_aliases(decl_types, type) do
    case type do
      %NamedType{:name => name} ->
        case decl_types[name] do
          %AliasDeclType{:aliases => aliases} ->
            resolve_aliases(decl_types, aliases)

          _ -> type
        end

      _ -> type
    end
  end
end
