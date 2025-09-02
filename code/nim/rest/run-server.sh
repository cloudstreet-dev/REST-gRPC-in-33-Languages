#!/bin/bash

echo "Starting Nim REST API Server..."

# Check if Nim is installed
if ! command -v nim &> /dev/null; then
    echo "Error: Nim is not installed"
    echo "Please install Nim:"
    echo "  brew install nim  # macOS"
    echo "  curl https://nim-lang.org/choosenim/init.sh -sSf | sh  # Unix"
    echo "  Or visit: https://nim-lang.org/install.html"
    exit 1
fi

# Check if nimble is installed
if ! command -v nimble &> /dev/null; then
    echo "Error: Nimble is not installed"
    echo "Nimble usually comes with Nim. Please reinstall Nim."
    exit 1
fi

cd server

# Install dependencies and run
nimble install -y
nimble run