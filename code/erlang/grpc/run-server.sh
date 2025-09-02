#!/bin/bash

echo "Starting Erlang gRPC Server..."

# Check if rebar3 is installed
if ! command -v rebar3 &> /dev/null; then
    echo "Error: rebar3 is not installed"
    echo "Please install rebar3:"
    echo "  brew install rebar3  # macOS"
    echo "  Or visit: https://rebar3.org"
    exit 1
fi

cd server

# Get dependencies and compile
echo "Building server..."
rebar3 compile

# Generate gRPC code from proto files
rebar3 grpc gen

# Run the server
echo "Starting gRPC server on port 50051..."
rebar3 shell