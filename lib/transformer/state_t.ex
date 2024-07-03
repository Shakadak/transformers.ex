defmodule Transformer.StateT do
  # Transformer.Monad.Trans.State.Strict

  # rwsT :: s -> m (a, s)) -> StateT s m a
  defmacro mkStateT(f) do
    f
  end

  @doc """
  Unwrap an StateT computation as a function.
  (The inverse of `rwsT`.)

  ```
  runStateT :: StateT s m a -> s -> m {a, s}
  ```
  """
  #def runStateT(m, s), do: m.runStateT.(s |> tap(&IO.inspect(get_in(&1, [Access.key!(:varMap), 0, Access.key(:values)]), label: "runStateT(_, s)")))
  defmacro runStateT(m, s), do: (quote do unquote(m).(unquote(s)) end)

  defmacro mk(dict) do
    require_ast = 
      if is_atom(Macro.expand_literals(dict, __CALLER__)) do
        quote do require unquote(dict) end
      end

    quote location: :keep do

      unquote(require_ast)

      require Transformer.StateT

      @doc """
      Evaluate a computation with the given initial state,
      returning the final value, discarding the final state.

      ```
      evalStateT :: Monad m
               => StateT s m a     -- ^computation to execute
               -> s                -- ^initial value
               -> m {a, [w]}      -- ^computation yielding final value and output
      ```
      """
      def evalStateT(m, s) do
        Transformer.StateT.runStateT(m, s)
        |> unquote(dict).bind(fn {a, _} -> unquote(dict).pure(a) end)
      end

      @doc """
      Evaluate a computation with the given initial state,
      returning the final state, discarding the final value.

      ```
      execStateT :: Monad m
               => StateT s m a  -- ^computation to execute
               -> s               -- ^initial value
               -> m {s, [w]}      -- ^computation yielding final state and output
      ```
      """
      def execStateT(m, s) do
        Transformer.StateT.runStateT(m, s)
        |> unquote(dict).bind(fn {_, s2} -> unquote(dict).pure(s2) end)
      end

      @doc """
      Map the inner computation using the given function.

      ```
      runStateT(mapStateT(m, f), s) = f.(runStateT(m, s))
      ```

      ```
      mapStateT :: (Monad m, Monad n) => StateT s m a -> (m (a, s, [w]) -> n (b, s, [w'])) -> MonadDict n -> StateT s n b
      ```
      """
      def mapStateT(m, f) do
        Transformer.StateT.mkStateT(fn s -> f.(Transformer.StateT.runStateT(m, s)) end)
      end

      @doc """
      `withStateT(m, f)` executes action `m` on a state modified by applying `f`.

      ```
      withStateT :: StateT s m a -> (s -> s) -> StateT s m a
      ```
      """
      def withStateT(m, f) do
        Transformer.StateT.mkStateT fn s -> Transformer.StateT.runStateT(m, f.(s)) end
      end

      ### Functor

      # map :: StateT s a, (a -> b) -> StateT s b
      def map(m, f) do
        Transformer.StateT.mkStateT fn s ->
          unquote(dict).map(Transformer.StateT.runStateT(m, s), fn {a, s2} -> {f.(a), s2} end)
        end
      end

      ### Applicative

      # pure :: a -> StateT s a
      def pure(a) do
        Transformer.StateT.mkStateT fn s -> unquote(dict).pure({a, s}) end
      end

      def ap(mf, mx) do
        Transformer.StateT.mkStateT fn s ->
          Transformer.StateT.runStateT(mf, s)
          |> unquote(dict).bind(fn {f, s1} ->
            Transformer.StateT.runStateT(mx, s1)
            |> unquote(dict).bind(fn {x, s2} ->
              unquote(dict).pure({f.(x), s2})
            end)
          end)
        end
      end

      require Default.Applicative ; Default.Applicative.mk()

      ### Monad

      def bind(m, k) do
        Transformer.StateT.mkStateT fn s ->
          Transformer.StateT.runStateT(m, s)
          |> unquote(dict).bind(fn {a, s2} -> Transformer.StateT.runStateT(k.(a), s2) end)
        end
      end

      require Default.Monad ; Default.Monad.mk()

      ### MonadTrans

      def lift(m) do
        Transformer.StateT.mkStateT fn s ->
          m |> unquote(dict).bind(fn a -> unquote(dict).pure {a, s} end)
        end
      end

      ### State operations  ------------------------------------------------------------------

      @doc """
      Construct a state monad computation from a state transformer function.

      ```
      state :: Monad m => (s -> (a, s)) -> RWST r w s m a
      ```
      """
      def state(f) do
        Transformer.StateT.mkStateT fn s -> unquote(dict).pure(f.(s)) end
      end

      @doc """
      Fetch the current value of the state within the monad.

      ```
      get :: Monad m => RWST r w s m s
      ```
      """
      def get, do: gets(& &1)

      @doc """
      `put(s)` sets the state within the monad to `s`.

      ```
      put :: (Monad m) => s -> RWST r w s m ()
      ```
      """
      def put(s) do
        state fn _ -> {{}, s} end
      end

      @doc """
      `modify(f)` is an action that updates the state to the result of
      applying `f` to the current state.

      ```
      modify(f) = get() |> bind(&put(f.(&1)))
      ```

      ```
      modify :: Monad m => (s -> s) -> RWST r w s m ()
      ```
      """
      def modify(f) do
        state fn s -> {{}, f.(s)} end
      end

      @doc """
      Get a specific component of the state, using a projection function
      supplied.

      ```
      gets(f) = map(get(), f)
      ```

      ```
      gets :: (Monad m) => (s -> a) -> RWST r w s m a
      ```
      """
      def gets(f) do
        state fn s -> {f.(s), s} end
      end

      ### Generic operations -----------------------------------------------------------------

      def mapM([], _), do: pure([])
      def mapM([x | xs], f) do
        f.(x) |> bind(fn y ->
          (mapM xs, f) |> bind(fn ys ->
            pure [y | ys]
          end)
        end)
      end

      def mapM_([], _), do: pure({})
      def mapM_([x | xs], f) do
        f.(x) |> bind(fn _ -> mapM_ xs, f end)
      end

      def replicateM(0, _), do: pure([])
      def replicateM(n, m) when is_integer(n) and n > 0 do
        m |> bind(fn x ->
          (replicateM n - 1, m) |> bind(fn xs ->
            pure [x | xs]
          end)
        end)
      end

      defmacro whenM(cnd, m) do
        quote do
          case unquote(cnd) do
            true -> unquote(m)
            false -> unquote(__MODULE__).pure({})
          end
        end
      end

      ### Monad Plus -------------------------------------------------------------------------

      if function_exported?(unquote(dict), :mzero, 0) or macro_exported?(unquote(dict), :mzero, 0) do
      @doc """
      Applicable only if the parent class has a MonadPlus instance
      """
      def mzero do
        Transformer.StateT.mkStateT fn _ -> unquote(dict).mzero() end
      end
      end

      if function_exported?(unquote(dict), :mplus, 2) or macro_exported?(unquote(dict), :mplus, 2) do
      @doc """
      Applicable only if the parent class has a MonadPlus instance
      """
      def mplus(ml, mr) do
        Transformer.StateT.mkStateT fn s ->
          Transformer.StateT.runStateT(ml, s) |> unquote(dict).mplus(Transformer.StateT.runStateT(mr, s))
        end
      end
      end

      if Module.defines?(__MODULE__, {:mzero, 0}) do
      def guard(true), do: pure {}
      def guard(false), do: mzero()
      end
    end
  end
end
