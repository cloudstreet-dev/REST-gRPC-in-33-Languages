defmodule TaskGrpcClient.MixProject do
  use Mix.Project

  def project do
    [
      app: :task_grpc_client,
      version: "0.1.0",
      elixir: "~> 1.14",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      escript: [main_module: TaskGrpcClient.CLI]
    ]
  end

  def application do
    [
      extra_applications: [:logger]
    ]
  end

  defp deps do
    [
      {:grpc, "~> 0.7"},
      {:protobuf, "~> 0.11"},
      {:google_protos, "~> 0.3"}
    ]
  end
end