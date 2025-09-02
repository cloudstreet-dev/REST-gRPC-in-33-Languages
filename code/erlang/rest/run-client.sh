#!/bin/bash

echo "Running Erlang REST API Client Demo..."

# Check if Erlang is installed
if ! command -v erl &> /dev/null; then
    echo "Error: Erlang is not installed"
    echo "Please install Erlang:"
    echo "  brew install erlang  # macOS"
    echo "  apt-get install erlang  # Ubuntu/Debian"
    echo "  Or visit: https://www.erlang.org/downloads"
    exit 1
fi

# Check if rebar3 is installed
if ! command -v rebar3 &> /dev/null; then
    echo "Error: rebar3 is not installed"
    echo "Installing rebar3..."
    wget https://s3.amazonaws.com/rebar3/rebar3 && chmod +x rebar3
    sudo mv rebar3 /usr/local/bin/
fi

# Wait for server to be ready
echo "Waiting for server to start..."
sleep 2

cd client

# Get dependencies and compile
rebar3 compile

# Build and run escript
rebar3 escriptize
./_build/default/bin/task_client