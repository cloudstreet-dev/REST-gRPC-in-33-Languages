#!/bin/bash

echo "Starting Clojure REST API Server..."

# Check if Leiningen is installed
if ! command -v lein &> /dev/null; then
    echo "Error: Leiningen is not installed"
    echo "Please install Leiningen:"
    echo "  brew install leiningen  # macOS"
    echo "  Or visit: https://leiningen.org/#install"
    exit 1
fi

cd server
lein run