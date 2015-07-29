defmodule Millc.SSA2Bytecode do
  defmodule Builder do
    defmodule Context do
      @derive [Access]
      defstruct local_count: nil, local_map: nil, bytecode: nil
    end

    def start_link() do
      Agent.start_link(fn() ->
        %Context{local_count: 0, local_map: %{}, bytecode: []}
      end)
    end

    def stop(builder) do
      Agent.stop(builder)
    end

    def get(builder) do
      Agent.get(builder, &({&1[:local_count], &1[:bytecode]}))
    end

    def append(builder, bytecode_instr) do
      Agent.update(builder, fn(ctx) ->
        update_in(ctx, [:bytecode], &(&1 ++ [bytecode_instr]))
      end)
    end

    def load(builder, instr_id) do
      Agent.update(builder, fn(ctx) ->
        bytecode_instr = %{opcode: "ldloc", index: ctx[:local_map][instr_id]}
        update_in(ctx, [:bytecode], &(&1 ++ [bytecode_instr]))
      end)
    end

    def store(builder, instr_id) do
      Agent.update(builder, fn(ctx) ->
        bytecode_instr = %{opcode: "stloc", index: ctx[:local_count]}
        ctx
        |> update_in([:local_count], &(&1 + 1))
        |> put_in([:local_map, instr_id], bytecode_instr[:index])
        |> update_in([:bytecode], &(&1 ++ [bytecode_instr]))
      end)
    end
  end

  def codegen(cfg) do
    {:ok, builder} = Builder.start_link()
    try do
      if Enum.count(cfg) !== 1 do
        raise "codegen for multiple blocks not yet implemented"
      end
      [{_, block} | _] = Dict.to_list(cfg)
      Enum.each(block, &codegen_instr(builder, &1))
      Builder.get(builder)
    after
      Builder.stop(builder)
    end
  end

  defp codegen_instr(builder, instr) do
    {instr_id, instr} = instr
    case instr do
      {:call, callee, args} ->
        Builder.load(builder, callee)
        Enum.each(args, &Builder.load(builder, &1))
        Builder.append(builder, %{opcode: "call", arguments: length(args)})
        Builder.store(builder, instr_id)

      {:goto, _block_id} ->
        raise "not implemented"

      {:if, _condition, _then_id, _else_id} ->
        raise "not implemented"

      {:ldarg, index} ->
        Builder.append(builder, %{opcode: "ldarg", argument: index})
        Builder.store(builder, instr_id)

      {:ldint, _value} ->
        raise "not implemented"

      {:ldgbl, module_name, name} ->
        full_name = "#{module_name |> Enum.join(".")}.#{name}"
        Builder.append(builder, %{opcode: "ldgbl", name: full_name})
        Builder.store(builder, instr_id)

      {:ldstr, value} ->
        Builder.append(builder, %{opcode: "ldstr", value: value})
        Builder.store(builder, instr_id)

      {:new, type} ->
        Builder.append(builder, %{opcode: "new", type: Millc.Type.descriptor(type)})
        Builder.store(builder, instr_id)

      {:ret, value} ->
        Builder.load(builder, value)
        Builder.append(builder, %{opcode: "ret"})

      {:tailcall, callee, args} ->
        # TODO: Generate tailcall instruction.
        Builder.load(builder, callee)
        Enum.each(args, &Builder.load(builder, &1))
        Builder.append(builder, %{opcode: "call", arguments: length(args)})
        Builder.append(builder, %{opcode: "ret"})
    end
  end
end
