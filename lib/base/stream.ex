defmodule Base.Stream do
  def map(m, f), do: Stream.map(m, f)

  def pure(x), do: Stream.concat([[x]])

  def bind(m, f), do: Stream.flat_map(m, f)

  def mzero, do: Stream.concat([])

  def mplus(l, r), do: Stream.concat(l, r)


  ### COMPUTATION EXPRESSION ###

  def _Pure(x), do: pure(x)

  def _Bind(x, f), do: bind(x, f)

  def _PureFrom(m), do: m

  def _Zero, do: pure({})
end
