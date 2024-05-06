defmodule Data.ResultRWS do
  require Transformer.RwsT

  Transformer.RwsT.mk(Base.Result)

  def throwError(x), do: __MODULE__.lift(Base.Result.error(x))
end
