#!/bin/bash

echo "Running Gleam REST API Client Demo..."

# Check if Gleam is installed
if ! command -v gleam &> /dev/null; then
    echo "Error: Gleam is not installed"
    echo "Please install Gleam:"
    echo "  brew install gleam  # macOS"
    echo "  Or visit: https://gleam.run/getting-started/"
    exit 1
fi

# Wait for server to be ready
echo "Waiting for server to start..."
sleep 2

cd client

# Build and run the client
gleam run