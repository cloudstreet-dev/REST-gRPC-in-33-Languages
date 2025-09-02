#!/bin/bash

echo "Starting V REST API Server..."

# Check if V is installed
if ! command -v v &> /dev/null; then
    echo "Error: V is not installed"
    echo "Please install V:"
    echo "  git clone https://github.com/vlang/v"
    echo "  cd v && make"
    echo "  Or visit: https://vlang.io"
    exit 1
fi

cd server

# Run the server
echo "Starting server on port 8080..."
v run src/main.v