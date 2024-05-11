defmodule Wrapped.ResultState do
  require Transformer.StateT

  Transformer.StateT.mk(Base.Result)

  def throwError(x), do: __MODULE__.lift(Base.Result.error(x))
end
