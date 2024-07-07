defmodule Default.Applicative do
  defmacro mk do
    quote location: :keep do
      import Transformers.Internal.Macros

      locally_default {:ap, 2} do
        def ap(mf, mx), do: liftA2(& &1.(&2), mf, mx)
      end

      locally_default {:liftA2, 3} do
        def liftA2(f, mx, my), do: map(mx, &fn y -> f.(&1, y) end) |> ap(my)
      end

      locally_default {:leftA, 2} do
        def leftA(ml, mr), do: liftA2(fn l, _r -> l end, ml, mr)
      end

      locally_default {:rightA, 2} do
        def rightA(ml, mr), do: liftA2(fn _l, r -> r end, ml, mr)
      end



      locally_default {:whenM, 2} do
        defmacro whenM(cnd, m) do
          quote do
            case unquote(cnd) do
              true -> unquote(m)
              false -> unquote(__MODULE__).pure({})
            end
          end
        end
      end
    end
  end
end
