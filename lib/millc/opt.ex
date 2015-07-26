defmodule Millc.Opt do
  use Behaviour

  # TODO: proper types.
  defcallback optimize(cfg :: any) :: any
end
