#!/bin/bash

echo "Running V REST API Client Demo..."

# Check if V is installed
if ! command -v v &> /dev/null; then
    echo "Error: V is not installed"
    echo "Please install V:"
    echo "  git clone https://github.com/vlang/v"
    echo "  cd v && make"
    echo "  Or visit: https://vlang.io"
    exit 1
fi

# Wait for server to be ready
echo "Waiting for server to start..."
sleep 2

cd client

# Run the client
v run src/main.v