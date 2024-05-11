defmodule Wrapped.StreamState do
  require Transformer.StateT

  Transformer.StateT.mk(Base.Stream)
end
