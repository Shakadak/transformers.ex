defmodule Transformers.Internal.Macros do
  defmacro optional(dict, {name, arity}, do: block) do
    quote do
      if function_exported?(unquote(dict), unquote(name), unquote(arity)) or macro_exported?(unquote(dict), unquote(name), unquote(arity)) do
        unquote(block)
      end
    end
  end

  defmacro locally_optional({_name, _arity} = sig, do: block) do
    quote do
      if Module.defines?(__MODULE__, unquote(sig)) do
        unquote(block)
      end
    end
  end
end
