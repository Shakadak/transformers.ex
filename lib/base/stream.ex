defmodule Base.Stream do
  def map(m, f), do: Stream.map(m, f)

  def pure(x), do: Stream.concat([[x]])

  def bind(m, f), do: Stream.flat_map(m, f)

  def mzero, do: Stream.concat([])

  def mplus(l, r), do: Stream.concat(l, r)
end
