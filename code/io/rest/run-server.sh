#!/bin/bash

echo "Starting Io REST API Server..."

# Check if Io is installed
if ! command -v io &> /dev/null; then
    echo "Error: Io is not installed"
    echo "Please install Io:"
    echo "  brew install io  # macOS"
    echo "  Or visit: https://iolanguage.org"
    exit 1
fi

cd server

# Run the server
echo "Starting server on port 8080..."
io server.io