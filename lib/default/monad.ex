defmodule Default.Monad do
  defmacro mk do
    quote do
      unless Module.defines?(__MODULE__, {:leftM, 2}) do
        def leftM(ml, mr), do: bind(ml, fn _ -> mr end)
      end

      unless Module.defines?(__MODULE__, {:leftM, 2}) do
        def rightM(ml, mr), do: bind(ml, fn l -> bind(mr, fn _ -> pure(l) end) end)
      end
    end
  end
end
