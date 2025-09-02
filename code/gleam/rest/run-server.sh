#!/bin/bash

echo "Starting Gleam REST API Server..."

# Check if Gleam is installed
if ! command -v gleam &> /dev/null; then
    echo "Error: Gleam is not installed"
    echo "Please install Gleam:"
    echo "  brew install gleam  # macOS"
    echo "  Or visit: https://gleam.run/getting-started/"
    exit 1
fi

cd server

# Build and run the server
echo "Building and running server..."
gleam run