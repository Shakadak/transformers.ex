defmodule Base.Identity do
  defmacro map(x, f) do
    quote do
      unquote(f).(unquote(x))
    end
  end

  defmacro pure(x), do: x

  defmacro ap(f, x) do
    quote do
      unquote(f).(unquote(x))
    end
  end

  defmacro liftA2(f, x, y) do
    quote do
      unquote(f).(unquote(x), unquote(y))
    end
  end

  defmacro bind(x, f) do
    quote do
      unquote(f).(unquote(x))
    end
  end

  ### COMPUTATION EXPRESSION ###

  defmacro _Pure(x), do: (quote do unquote(__MODULE__).pure(unquote(x)) end)

  defmacro _Bind(m, f), do: (quote do unquote(__MODULE__).bind(unquote(m), unquote(f)) end)

  defmacro _PureFrom(m), do: m

  defmacro _Zero, do: (quote do {} end)
end
