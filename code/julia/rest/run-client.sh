#!/bin/bash

echo "Running Julia REST API Client Demo..."

# Check if Julia is installed
if ! command -v julia &> /dev/null; then
    echo "Error: Julia is not installed"
    echo "Please install Julia:"
    echo "  brew install julia  # macOS"
    echo "  Or visit: https://julialang.org/downloads/"
    exit 1
fi

# Wait for server to be ready
echo "Waiting for server to start..."
sleep 2

cd client

# Install dependencies
echo "Installing dependencies..."
julia -e 'using Pkg; Pkg.activate("."); Pkg.instantiate()'

# Run the client
julia --project=. client.jl