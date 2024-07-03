defmodule Default.Applicative do
  defmacro mk do
    quote location: :keep do
      unless Module.defines?(__MODULE__, {:ap, 2}) do
        def ap(mf, mx), do: liftA2(& &1.(&2), mf, mx)
      end

      unless Module.defines?(__MODULE__, {:liftA2, 3}) do
        def liftA2(f, mx, my), do: map(mx, &fn y -> f.(&1, y) end) |> ap(my)
      end

      unless Module.defines?(__MODULE__, {:leftA, 2}) do
        def leftA(ml, mr), do: liftA2(fn l, _r -> l end, ml, mr)
      end

      unless Module.defines?(__MODULE__, {:rightA, 2}) do
        def rightA(ml, mr), do: liftA2(fn _l, r -> r end, ml, mr)
      end
    end
  end
end
