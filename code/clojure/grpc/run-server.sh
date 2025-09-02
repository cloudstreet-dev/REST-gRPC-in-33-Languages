#!/bin/bash

echo "Starting Clojure gRPC Server..."

# Check if Leiningen is installed
if ! command -v lein &> /dev/null; then
    echo "Error: Leiningen is not installed"
    echo "Please install Leiningen:"
    echo "  brew install leiningen  # macOS"
    echo "  Or visit: https://leiningen.org"
    exit 1
fi

cd server

# Run the server
echo "Starting gRPC server on port 50051..."
lein run