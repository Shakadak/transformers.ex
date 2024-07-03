defmodule Base.List do
  def map(m, f), do: Enum.map(m, f)

  def pure(x), do: [x]

  def bind(m, f), do: Enum.flat_map(m, f)

  def mzero, do: []

  def mplus(l, r), do: l ++ r
end
