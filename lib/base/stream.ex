defmodule Base.Stream do
  defmacro map(m, f), do: (quote do Stream.map(unquote(m), unquote(f)) end)

  defmacro pure(x), do: (quote do [unquote(x)] end)

  defmacro bind(m, f), do: (quote do Stream.flat_map(unquote(m), unquote(f)) end)

  defmacro mzero, do: (quote do [] end)

  defmacro mplus(l, r), do: (quote do Stream.concat(unquote(l), unquote(r)) end)
end
