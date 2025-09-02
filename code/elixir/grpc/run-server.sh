#!/bin/bash

echo "Starting Elixir gRPC Server..."
cd server
mix deps.get
mix compile
mix run --no-halt