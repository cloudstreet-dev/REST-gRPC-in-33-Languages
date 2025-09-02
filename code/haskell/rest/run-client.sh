#!/bin/bash

echo "Running Haskell REST API Client Demo..."

# Check if GHC is installed
if ! command -v ghc &> /dev/null; then
    echo "Error: GHC is not installed"
    echo "Please install GHC and Cabal:"
    echo "  curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh"
    exit 1
fi

cd client

# Build and run
if command -v cabal &> /dev/null; then
    cabal build
    cabal run task-client
elif command -v stack &> /dev/null; then
    stack build
    stack exec task-client
else
    echo "Error: Neither Cabal nor Stack found"
    echo "Please install Cabal or Stack"
    exit 1
fi