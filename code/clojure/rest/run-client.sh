#!/bin/bash

echo "Running Clojure REST API Client Demo..."

# Check if Leiningen is installed
if ! command -v lein &> /dev/null; then
    echo "Error: Leiningen is not installed"
    echo "Please install Leiningen:"
    echo "  brew install leiningen  # macOS"
    echo "  Or visit: https://leiningen.org/#install"
    exit 1
fi

cd client
lein run