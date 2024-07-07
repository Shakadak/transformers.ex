defmodule ResultT do
  defmacro mk(dict) do
    quote location: :keep do

      require Transformers.Internal.Macros

      Transformer.Internal.Macros.require_ast(unquote(dict))

      ### Functor

      def map(m, f) do
        unquote(dict).map(m, &Base.Result.map(&1, f))
      end

      ### Applicative

      def pure(a) do
        unquote(dict).pure(Base.Result.ok(a))
      end

      def ap(f, v) do
        f |> unquote(dict).bind(fn mf ->
          case mf do
            error(e) -> unquote(dict).pure(error(e))
            ok(k) ->
              v |> unquote(dict).bind(fn mv ->
                case mv do
                  error(e) -> unquote(dict).pure(error(e))
                  ok(v) -> unquote(dict).pure(ok(f.(v)))
                end
              end)
          end
        end)
      end

      def rightA(m, k), do: (m |> unquote(dict).bind(fn _ -> k end))

      require Default.Applicative ; Default.Applicative.mk()

      ### Monad

      def bind(m, k) do
        m |> unquote(dict).bind(fn a ->
          case a do
            error(e) -> unquote(dict).pure(error(e))
            ok(x) -> k.(x)
          end
        end)
      end

      require Default.Monad ; Default.Monad.mk()

      ### MonadTrans

      def lift(m) do
        unquote(dict).liftM(m, &Base.Result.ok/1)
      end

      ### Other

      def throwE(e) do
        unquote(dict).pure(error(e))
      end

      def catchE(m, h) do
        m |> unquote(dict).bind(fn a ->
          case a do
            error(l) -> h.(l)
            ok(r) -> unquote(dict).pure(ok(r))
          end
        end)
      end

      def handleE(h, m), do: catchE(m, h)

      def tryE(m) do
        catchE(liftM(&Base.Result.ok/1), &pure(Base.Result.error(&1)))
      end

      def finallyE(m, closer) do
        tryE(m) |> bind(fn res ->
          closer |> bind(fn _ ->
            res |> Base.Result.either(&throwE/1, &pure/1)
          end)
        end)
      end
    end
  end
end
