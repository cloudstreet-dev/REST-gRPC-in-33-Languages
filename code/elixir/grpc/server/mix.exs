defmodule TaskGrpcServer.MixProject do
  use Mix.Project

  def project do
    [
      app: :task_grpc_server,
      version: "0.1.0",
      elixir: "~> 1.14",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  def application do
    [
      extra_applications: [:logger],
      mod: {TaskGrpcServer.Application, []}
    ]
  end

  defp deps do
    [
      {:grpc, "~> 0.7"},
      {:protobuf, "~> 0.11"},
      {:google_protos, "~> 0.3"},
      {:uuid, "~> 1.1"}
    ]
  end
end