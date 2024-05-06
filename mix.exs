defmodule Transformers.MixProject do
  use Mix.Project

  def project do
    [
      app: :transformers,
      version: "0.1.0",
      elixir: "~> 1.16",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger]
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:computation_expression, git: "https://github.com/Shakadak/computation_expression.ex"},
      # {:computation_expression, path: "../computation_expression"},
    ]
  end
end
