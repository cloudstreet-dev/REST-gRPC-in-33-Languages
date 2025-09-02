#!/bin/bash

echo "Running Nim REST API Client Demo..."

# Check if Nim is installed
if ! command -v nim &> /dev/null; then
    echo "Error: Nim is not installed"
    echo "Please install Nim:"
    echo "  brew install nim  # macOS"
    echo "  curl https://nim-lang.org/choosenim/init.sh -sSf | sh  # Unix"
    echo "  Or visit: https://nim-lang.org/install.html"
    exit 1
fi

# Wait for server to be ready
echo "Waiting for server to start..."
sleep 2

cd client

# Install dependencies and run
nimble install -y
nimble run