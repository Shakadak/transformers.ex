defmodule Base.Result do
  @moduledoc """
  The Result type represents values with two possibilities: an error value, or a correct value.
  This is typically used to represent a happy path, where `ok/1` represents continuation, and `error/1` represents early return.
  We will call this a workflow.
  """

  @type t(error, ok) :: {:error, error} | {:ok, ok}

  alias ComputationExpression, as: CE
  require CE

  @doc """
  Abstracts away the representation of the ok constructor.
  """
  defmacro ok(x), do: quote(do: {:ok, unquote(x)})

  @doc """
  Abstracts away the representation of the error constructor.
  """
  defmacro error(x), do: quote(do: {:error, unquote(x)})

  def assert_result!(ok(_) = x), do: x
  def assert_result!(error(_) = x), do: x
  def assert_result!(other), do: raise(ArgumentError, message: "Expected #{inspect(__MODULE__)} but instead got #{inspect(other)}.")

  @doc """
  Converts a simple ok / error 2-tuple into a `Result.t`

      iex> Data.Result.from_raw({:ok, 1})
      Data.Result.ok(1)
      iex> Data.Result.from_raw({:error, 2})
      Data.Result.error(2)
  """
  def from_raw({:ok, x}), do: ok(x)
  def from_raw({:error, x}), do: error(x)

  @doc """
  Converts a `Result.t` into a simple ok / error 2-tuple

      iex> Data.Result.to_raw(Data.Result.ok(1))
      {:ok, 1}
      iex> Data.Result.to_raw(Data.Result.error(2))
      {:error, 2}
  """
  def to_raw(ok(x)), do: {:ok, x}
  def to_raw(error(x)), do: {:error, x}

  @doc """
  Case analysis for the Result type.
  Applies the first function in case of an error.
  Applies the secund function in case of an ok.
  Returns the result of the application.

      iex> import Data.Result
      iex> ok(0) |> either(& &1 - 1, & &1 + 1)
      1
      iex> error(0) |> either(& &1 - 1, & &1 + 1)
      -1
  """
  def either(error(a), f, _), do: f.(a)
  def either(ok(b), _, g), do: g.(b)

  @doc """
  Lift a value into a result.
  Equivalent to using `ok/1` directly, but you don't need to require the module.

  This should be viewed as placing a value into a successful context. Allowing
  further work on it whithin the workflow.

  Also part of a pattern.
  Also equivalent to `Enum.into/2`

      iex> Data.Result.pure({})
      Data.Result.ok({})
  """
  def pure(x), do: ok(x)

  @doc """
  Applies the function in case of an ok, leave the error untouched otherwise.
  This is the mose basic representation of the workflow defined by `Result`.

  This should be viewed as applying a function only in successful context,
  just like using `Enum.map/2` on a list should be viewed as applying a function on
  multiple possible values.

  Also part of a pattern.

      iex> import Data.Result
      iex> map(ok(0), & &1 + 1)
      ok(1)
      iex> map(error(0), & &1 + 1)
      error(0)
  """
  def map(ok(x), f), do: ok(f.(x))
  def map(error(y), _), do: error(y)

  @doc """
  This is the same as map, but allows to transform the data contained inside the error.

  This should be viewed as way to do transformation on data without touching the workflow
  directed by the `Result`.
  Mostly useful when we want to normalize an error's content.

      iex> Data.Result.ok(0) |> Data.Result.bimap(fn {kind, payload, stacktrace} -> Exception.format(kind, payload, stacktrace) end, fn x -> x + 1 end)
      Data.Result.ok(1)
      iex> Data.Result.error({:error, :there_was_an_error, []}) |> Data.Result.bimap(fn {kind, payload, stacktrace} -> Exception.format(kind, payload, stacktrace) end, fn x -> x + 1 end)
      Data.Result.error("** (ErlangError) Erlang error: :there_was_an_error")
  """
  def bimap(error(a), f, _), do: error(f.(a))
  def bimap(ok(b), _, g), do: ok(g.(b))

  @doc """
  The composition of `map/2` and `join/1`.
  The bread and butter of the workflow defined by `Result`.
  This allows you to chain functions that return themselves `Result`s.

  This should be viewed as applying a function only in a successful context,
  but the context may change depending on the result of the function, and once we encounter an error,
  we may not go back to a successful one without external help.
  Just like using `Enum.flat_map/2` on a list should be viewed as applying a function
  on multiple possible values, but the amount a possible values may change overall.

  Also part of a pattern.

      iex> import Data.Result
      iex> succeed = &pure(&1 * 10)
      iex> fail = &error(&1 / 10)
      iex> val = pure(1)
      iex(1)> val |> bind(succeed) |> bind(succeed)
      ok(100)
      iex(2)> val |> bind(fail) |> bind(succeed)
      error(0.1)
      iex(3)> val |> bind(succeed) |> bind(fail)
      error(1.0)
      iex(4)> error(:nada) |> bind(succeed) |> bind(succeed)
      Data.Result.error(:nada)
  """
  def bind(ok(x), f), do: f.(x)
  def bind(error(_) = y, _), do: y

  @doc false
  # For the computation expression builder
  def return(x), do: pure(x)

  @doc """
  Allows you to collaps nested `ok`s

  This is rarely used as `bind/2` will mostly be used instead of `map/2`.
  This should be viewed as merging two level of Result into one, where only both being
  successful will result in a successful context.
  Just like using `Enum.concat/1` on a list should be viewed as merging two level
  of possible values into one level, and merging empty lists won't produce values.

      iex> import Data.Result
      iex> join(pure(pure(100)))
      ok(100)
      iex> join(error(100))
      error(100)

      iex> import Data.Result
      iex> succeed = &pure(&1 * 10)
      iex> fail = &error(&1 / 10)
      iex> val = pure(1)
      iex> val |> map(succeed) |> join()
      ok(10)
      iex> val |> map(fail) |> join()
      error(0.1)
      iex> error(:nada) |> map(succeed) |> join()
      error(:nada)
  """
  def join(ok(x)), do: x
  def join(error(_) = y), do: y

  @doc """
  Forcefully extract a value from an `ok`.
  Will crash on `error`s.

      iex> Data.Result.from_ok!(Data.Result.pure(13))
      13
  """
  def from_ok!(ok(x)), do: x

  @doc """
  Forcefully extract a value from an `error`.
  Will crash on `ok`s.

      iex> Data.Result.from_error!(Data.Result.error(:bonk))
      :bonk
  """
  def from_error!(error(x)), do: x

  @doc """
  Convert a list of `Result`s into a `Result` of a list.
  This follows the workflow defined by `Result`, so if an `error` is
  encountered the computation stops and this `error` is returned instead.

  This should be viewed as transposing the workflow brought by a list with
  the workflow brought by `Result`.

      iex> import Data.Result
      iex> sequence([ok(1), ok(2), ok(3)])
      ok([1, 2, 3])
      iex> sequence([ok(1), error(2), ok(3)])
      error(2)
      iex> sequence([ok(1), error(2), error(3)])
      error(2)
  """
  def sequence(mxs), do: mapM(mxs, & &1)

  @doc """
  Maps a function returning a `Result` to a list of values and converts it
  into a `Result` of a list. As `sequence/1`, stops on the first `error` encountered.

  Just as `sequence/1`, this should be viewed as wanting to use the list workflow of dealing with multiple
  possible values, with the `Result` workflow of keeping computation within a successful context.
  The mapping function will decide whether to keep going with the multiple values or not.

      iex> import Data.Result
      iex> [1, 2, 3] |> mapM(&Data.Result.pure/1)
      ok([1, 2, 3])
      iex> [1, 2, 3] |> mapM(fn x when rem(x, 2) == 0 -> error(x) ; x -> pure(x) end)
      error(2)
  """
  def mapM([], _), do: pure([])
  def mapM([x | xs], f) do
    CE.compute __MODULE__ do
      let! y = f.(x)
      let! ys = mapM(xs, f)
      pure [y | ys]
    end
  end

  @doc """
  Zip two lists together within the Result workflow. Meaning that
  if one of the zip fails, the whole zipping will fail.
  """
  def zipWithM([], _, _), do: pure([])
  def zipWithM(_, [], _), do: pure([])
  def zipWithM([x | xs], [y | ys], f) do
    CE.compute __MODULE__ do
      let! z = f.(x, y)
      let! zs = zipWithM(xs, ys, f)
      pure [z | zs]
    end
  end

  @doc """
  Reduce on a list of values, but stops at the first error encountered, as defined by
  the workflow of `Result`.

  This should be viewed in the same manner as `mapM/2`. This is an equivalent to `Enum.reduce_while/3`
  where the `Result` workflow decides whether to keep going with the rest of the reduction.

      iex> import Data.Result
      iex> reduceM([1, 2, 3], 0, &pure(&1 + &2))
      ok(6)
      iex> reduceM([1, 2, 3], 0, fn x, _acc when rem(x, 2) == 0 -> error(x) ; x, acc -> pure(x + acc) end)
      error(2)
  """
  # foldl_m :: (Monad m) => [a], b, (a, b -> m b) -> m b
  #def foldl_m(xs, z0, f) do
  #  c = fn x, k -> fn acc -> f.(x, acc) |> bind(k) end end
  #  List.foldr(xs, &pure/1, c).(z0)
  #end
  @spec reduceM(Enumerable.t(a), b, (a, b -> t(e, b))) :: t(e, b) when a: var, b: var, e: var
  def reduceM(xs, z0, f) do
    assert_result! Enum.reduce(xs, pure(z0), fn x, macc -> bind(macc, fn acc -> f.(x, acc) end) |> IO.inspect(label: "bind") end)
  end

  ### COMPUTATION EXPRESSION ###

  def _Pure(x), do: pure(x)

  def _Bind(x, f), do: bind(x, f)

  def _PureFrom(m), do: m

  def _Zero, do: pure({})
end
