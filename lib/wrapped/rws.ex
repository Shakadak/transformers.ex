defmodule Wrapped.RWS do
  require Transformer.RwsT

  Transformer.RwsT.mk(Base.Identity)
end
