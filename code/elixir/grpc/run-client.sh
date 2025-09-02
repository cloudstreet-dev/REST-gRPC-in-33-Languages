#!/bin/bash

echo "Running Elixir gRPC Client Demo..."
cd client
mix deps.get
mix compile
mix escript.build
./task_grpc_client