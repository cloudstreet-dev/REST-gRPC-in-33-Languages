#!/bin/bash

echo "Starting Julia REST API Server..."

# Check if Julia is installed
if ! command -v julia &> /dev/null; then
    echo "Error: Julia is not installed"
    echo "Please install Julia:"
    echo "  brew install julia  # macOS"
    echo "  Or visit: https://julialang.org/downloads/"
    exit 1
fi

cd server

# Install dependencies
echo "Installing dependencies..."
julia -e 'using Pkg; Pkg.activate("."); Pkg.instantiate()'

# Run the server
julia --project=. server.jl