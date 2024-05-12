defmodule Base.List do
  def map(m, f), do: Enum.map(m, f)

  def pure(x), do: Enum.concat([[x]])

  def bind(m, f), do: Enum.flat_map(m, f)

  def mzero, do: Enum.concat([])

  def mplus(l, r), do: Enum.concat(l, r)
end
