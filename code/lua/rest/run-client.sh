#!/bin/bash

echo "Running Lua REST API Client Demo..."

# Check if lua is installed
if ! command -v lua &> /dev/null; then
    echo "Error: Lua is not installed"
    echo "Please install Lua and required dependencies:"
    echo "  brew install lua luarocks"
    echo "  luarocks install http"
    echo "  luarocks install lua-cjson"
    exit 1
fi

cd client
lua client.lua