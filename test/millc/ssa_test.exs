defmodule Millc.SSATest do
  use ExUnit.Case

  test "dump" do
    graph = %{
      "b1" => [
        {"i1", {:ldgbl, ["mill", "ord"], "__gt__"}},
        {"i2", {:ldarg, 0}},
        {"i3", {:ldarg, 1}},
        {"i4", {:goto, "b2"}},
      ],
      "b2" => [
        {"i5", {:call, "i1", ["i2", "i3"]}},
        {"i6", {:if, "i5", "b3", "b4"}},
      ],
      "b3" => [
        {"i7", {:ldint, 1}},
        {"i8", {:ret, "i7"}},
      ],
      "b4" => [
        {"i9", {:ldint, 0}},
        {"i10", {:ret, "i9"}},
      ],
    }
    # :ok = Millc.SSA.dump(graph)
  end
end
