defmodule Transformers.Internal.Macros do
  @moduledoc false

  defmacro require_ast(dict) do
    if is_atom(Macro.expand_literals(dict, __CALLER__)) do
      quote do require unquote(dict) end
    end
  end

  defmacro optional(dict, [{name, arity}], do: block) do
    optional_ast(dict, name, arity, block)
  end
  defmacro optional(dict, {name, arity}, do: block) do
    optional_ast(dict, name, arity, block)
  end

  def optional_ast(dict, name, arity, block) do
    quote do
      if function_exported?(unquote(dict), unquote(name), unquote(arity)) or macro_exported?(unquote(dict), unquote(name), unquote(arity)) do
        unquote(block)
      end
    end
  end

  defmacro local_req([{_name, _arity} = sig], do: block) do
    local_req_ast(sig, block)
  end
  defmacro local_req({_name, _arity} = sig, do: block) do
    local_req_ast(sig, block)
  end

  def local_req_ast(sig, block) do
    quote do
      if Module.defines?(__MODULE__, unquote(sig)) do
        unquote(block)
      end
    end
  end

  defmacro locally_default([{_name, _arity} = sig], do: block) do
    locally_default_ast(sig, block)
  end
  defmacro locally_default({_name, _arity} = sig, do: block) do
    locally_default_ast(sig, block)
  end

  def locally_default_ast(sig, block) do
    quote do
      unless Module.defines?(__MODULE__, unquote(sig)) do
        unquote(block)
      end
    end
  end
end
