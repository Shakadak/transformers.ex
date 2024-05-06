defmodule Base.Option do
  def __struct__ do
    %{
      __struct__: __MODULE__,
      some: nil,
      none: :none,
    }
  end

  def __struct__(kv) do
    case kv do
      [some: x] -> %{__struct__: __MODULE__, some: x}
      [none: :none] -> %{__struct__: __MODULE__, none: :none}
    end
  end

  @doc """
  Abstracts away the representation of the ok constructor.
  """
  defmacro some(x), do: quote(do: %unquote(__MODULE__){some: unquote(x)})

  @doc """
  Abstracts away the representation of the error constructor.
  """
  defmacro none, do: quote(do: %unquote(__MODULE__){none: :none})

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

  def inspect(some(x), opts), do: Inspect.Algebra.concat(["Data.Option.some(", Inspect.Algebra.to_doc(x, opts), ")"])
  def inspect(none(), _opts), do: Inspect.Algebra.concat(["Data.Result.none()"])
end
