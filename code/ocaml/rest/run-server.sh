#!/bin/bash

echo "Starting OCaml REST API Server..."

# Check if opam is installed
if ! command -v opam &> /dev/null; then
    echo "Error: opam is not installed"
    echo "Please install opam:"
    echo "  brew install opam  # macOS"
    echo "  apt-get install opam  # Ubuntu/Debian"
    echo "  Or visit: https://opam.ocaml.org/doc/Install.html"
    exit 1
fi

# Check if dune is installed
if ! opam list -i dune &> /dev/null; then
    echo "Installing dune..."
    opam install dune -y
fi

cd server

# Install dependencies
opam install . --deps-only -y

# Build and run
dune exec task_server