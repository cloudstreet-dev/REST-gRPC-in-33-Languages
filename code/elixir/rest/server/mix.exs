defmodule TaskServer.MixProject do
  use Mix.Project

  def project do
    [
      app: :task_server,
      version: "0.1.0",
      elixir: "~> 1.14",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  def application do
    [
      extra_applications: [:logger],
      mod: {TaskServer.Application, []}
    ]
  end

  defp deps do
    [
      {:plug_cowboy, "~> 2.5"},
      {:jason, "~> 1.4"},
      {:cors_plug, "~> 3.0"},
      {:uuid, "~> 1.1"}
    ]
  end
end