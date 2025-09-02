#!/bin/bash

echo "Running Io REST API Client Demo..."

# Check if Io is installed
if ! command -v io &> /dev/null; then
    echo "Error: Io is not installed"
    echo "Please install Io:"
    echo "  brew install io  # macOS"
    echo "  Or visit: https://iolanguage.org"
    exit 1
fi

# Wait for server to be ready
echo "Waiting for server to start..."
sleep 2

cd client

# Run the client
io client.io