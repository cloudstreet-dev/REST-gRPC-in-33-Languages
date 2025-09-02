#!/bin/bash

echo "Starting Erlang REST API Server..."

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

cd server

# Get dependencies and compile
rebar3 compile

# Start the server
rebar3 shell --apps task_server