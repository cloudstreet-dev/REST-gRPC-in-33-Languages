#!/bin/bash

echo "Running Clojure gRPC Client Demo..."

# Check if Leiningen is installed
if ! command -v lein &> /dev/null; then
    echo "Error: Leiningen is not installed"
    echo "Please install Leiningen:"
    echo "  brew install leiningen  # macOS"
    echo "  Or visit: https://leiningen.org"
    exit 1
fi

# Wait for server to be ready
echo "Waiting for server to start..."
sleep 2

cd client

# Run the client demo
lein run localhost 50051