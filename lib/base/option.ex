defmodule Base.Option do
  @enforce_keys [:tag, :value]
  defstruct @enforce_keys

  @doc """
  Abstracts away the representation of the some constructor.
  """
  defmacro some(x), do: quote(do: %unquote(__MODULE__){tag: :some, value: unquote(x)})

  @doc """
  Abstracts away the representation of the none constructor.
  """
  defmacro none, do: quote(do: %unquote(__MODULE__){tag: :none, value: nil})

  def map(some(x), f), do: some(f.(x))
  def map(none(),  _), do: none()

  def pure(x), do: some(x)

  def ap(some(f), some(x)), do: some(f.(x))
  def ap(_,       _      ), do: none()

  def liftA2(f, some(x), some(y)), do: some(f.(x, y))
  def liftA2(_, _      , _      ), do: none()

  def bind(some(x), f), do: f.(x)
  def bind(none(),  _), do: none()
end

defimpl Inspect, for: Base.Option do
  import Base.Option

  def inspect(some(x), opts), do: Inspect.Algebra.concat(["Base.Option.some(", Inspect.Algebra.to_doc(x, opts), ")"])
  def inspect(none(), _opts), do: Inspect.Algebra.concat(["Base.Option.none()"])
end
