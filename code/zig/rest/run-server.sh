#!/bin/bash

echo "Starting Zig REST API Server..."

# Check if Zig is installed
if ! command -v zig &> /dev/null; then
    echo "Error: Zig is not installed"
    echo "Please install Zig:"
    echo "  brew install zig  # macOS"
    echo "  Or visit: https://ziglang.org/download/"
    exit 1
fi

cd server
zig build run