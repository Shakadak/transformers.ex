defmodule Default.Monad do
  defmacro mk do
    quote do
      locally_default {:leftM, 2} do
        def leftM(ml, mr), do: bind(ml, fn _ -> mr end)
      end

      locally_default {:leftM, 2} do
        def rightM(ml, mr), do: bind(ml, fn l -> bind(mr, fn _ -> pure(l) end) end)
      end

      locally_default {:liftM, 2} do
        def liftM(mx, f), do: bind(mx, fn x -> pure(f.(x)) end)
      end

      locally_default mapM: 2 do
        def mapM([], _), do: pure([])
        def mapM([x | xs], f) do
          f.(x) |> bind(fn y ->
            (mapM xs, f) |> bind(fn ys ->
              pure [y | ys]
            end)
          end)
        end
      end

      locally_default mapM_: 2 do
        def mapM_([], _), do: pure({})
        def mapM_([x | xs], f) do
          f.(x) |> bind(fn _ -> mapM_ xs, f end)
        end
      end

      locally_default replicateM: 2 do
        def replicateM(0, _), do: pure([])
        def replicateM(n, m) when is_integer(n) and n > 0 do
          m |> bind(fn x ->
            (replicateM n - 1, m) |> bind(fn xs ->
              pure [x | xs]
            end)
          end)
        end
      end

      locally_default zipWithM: 3 do
        def zipWithM([], _, _), do: pure([])
        def zipWithM(_, [], _), do: pure([])
        def zipWithM([x | xs], [y | ys], f) do
          f.(x, y) |> bind(fn z ->
            (zipWithM xs, ys, f) |> bind(fn zs ->
              pure [z | zs]
            end)
          end)
        end
      end

      locally_default zipWithM_: 3 do
        def zipWithM_([], _, _), do: pure {}
        def zipWithM_(_, [], _), do: pure {}
        def zipWithM_([x | xs], [y | ys], f) do
          f.(x, y) |> bind(fn _ -> zipWithM_ xs, ys, f end)
        end
      end

    end
  end
end
