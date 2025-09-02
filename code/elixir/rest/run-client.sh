#!/bin/bash

echo "Running Elixir REST API Client Demo..."
cd client
mix deps.get
mix escript.build
./task_client