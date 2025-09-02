#!/bin/bash

echo "Starting Elixir REST API Server..."
cd server
mix deps.get
mix run --no-halt