defmodule Wrapped.State do
  require Transformer.StateT

  Transformer.StateT.mk(Base.Identity)
end
