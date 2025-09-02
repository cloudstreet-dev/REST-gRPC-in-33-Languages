#!/bin/bash

echo "Running Zig REST API Client Demo..."

# Check if Zig is installed
if ! command -v zig &> /dev/null; then
    echo "Error: Zig is not installed"
    echo "Please install Zig:"
    echo "  brew install zig  # macOS"
    echo "  Or visit: https://ziglang.org/download/"
    exit 1
fi

# Wait for server to be ready
echo "Waiting for server to start..."
sleep 2

cd client
zig build run