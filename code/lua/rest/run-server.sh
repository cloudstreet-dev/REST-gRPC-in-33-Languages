#!/bin/bash

echo "Starting Lua REST API Server..."

# Check if lua is installed
if ! command -v lua &> /dev/null; then
    echo "Error: Lua is not installed"
    echo "Please install Lua and required dependencies:"
    echo "  brew install lua luarocks"
    echo "  luarocks install http"
    echo "  luarocks install lua-cjson"
    echo "  luarocks install uuid"
    exit 1
fi

cd server
lua server.lua