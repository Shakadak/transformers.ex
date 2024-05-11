defmodule Transformer.StateT do
  # Transformer.Monad.Trans.State.Strict

  @enforce_keys [:runStateT]
  defstruct [:runStateT]

  # rwsT :: s -> m (a, s)) -> StateT s m a
  def new(f) when is_function(f, 1) do
    %__MODULE__{runStateT: f}
  end

  defmacro mk(dict) do
    require_ast = 
      if is_atom(Macro.expand_literals(dict, __CALLER__)) do
        quote do require unquote(dict) end
      end

    quote location: :keep do

      alias ComputationExpression, as: CE
      require CE

      unquote(require_ast)

      @doc """
      Unwrap an StateT computation as a function.
      (The inverse of `rwsT`.)

      ```
      runStateT :: StateT s a -> s -> Result {a, s}
      ```
      """
      def runStateT(m, s), do: m.runStateT.(s)

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
        CE.compute unquote(dict) do
          let! {a, _} = runStateT(m, s)
          pure a
        end
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
        CE.compute unquote(dict) do
          let! {_, s2} = runStateT(m, s)
          pure s2
        end
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
        Transformer.StateT.new(fn s -> f.(runStateT(m, s)) end)
      end

      @doc """
      `withStateT(m, f)` executes action `m` on a state modified by applying `f`.

      ```
      withStateT :: StateT s m a -> (s -> s) -> StateT s m a
      ```
      """
      def withStateT(m, f) do
        Transformer.StateT.new fn s ->
          runStateT(m, f.(s))
        end
      end

      # map :: StateT s a, (a -> b) -> StateT s b
      def map(m, f) do
        Transformer.StateT.new fn s ->
          unquote(dict).map(runStateT(m, s), fn {a, s2} -> {f.(a), s2} end)
        end
      end

      # pure :: a -> StateT s a
      def pure(a) do
        Transformer.StateT.new fn s ->
          unquote(dict).pure({a, s})
        end
      end

      def bind(m, k) do
        Transformer.StateT.new fn s ->
          CE.compute unquote(dict) do
            let! {a, s2} = runStateT(m, s)
            runStateT(k.(a), s2)
          end
        end
      end

      def lift(m) do
        Transformer.StateT.new fn s ->
          CE.compute unquote(dict) do
            let! a = m
            pure {a, s}
          end
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
        Transformer.StateT.new fn s -> unquote(dict).pure(f.(s)) end
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
        CE.compute __MODULE__ do
          let! y = f.(x)
          let! ys = mapM(xs, f)
          pure([y | ys])
        end
      end

      ### Monad Plus -------------------------------------------------------------------------

      @doc """
      Applicable only if the parent class has a MonadPlus instance
      """
      def mzero do
        dict = unquote(dict)
        Transformer.StateT.new fn _ -> dict.mzero() end
      end

      @doc """
      Applicable only if the parent class has a MonadPlus instance
      """
      def mplus(ml, mr) do
        dict = unquote(dict)
        Transformer.StateT.new fn s ->
          runStateT(ml, s) |> dict.mplus(runStateT(mr, s))
        end
      end

      ### COMPUTATION EXPRESSION ###

      def _Pure(x), do: pure(x)

      def _Bind(x, f), do: bind(x, f)

      def _PureFrom(m), do: m

      def _Zero, do: _Pure({})
    end
  end
end
