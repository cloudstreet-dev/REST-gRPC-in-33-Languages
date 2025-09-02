#!/bin/bash

echo "Running OCaml REST API Client Demo..."

# Check if opam is installed
if ! command -v opam &> /dev/null; then
    echo "Error: opam is not installed"
    echo "Please install opam:"
    echo "  brew install opam  # macOS"
    echo "  apt-get install opam  # Ubuntu/Debian"
    echo "  Or visit: https://opam.ocaml.org/doc/Install.html"
    exit 1
fi

# Wait for server to be ready
echo "Waiting for server to start..."
sleep 2

cd client

# Install dependencies
opam install . --deps-only -y

# Build and run
dune exec task_client