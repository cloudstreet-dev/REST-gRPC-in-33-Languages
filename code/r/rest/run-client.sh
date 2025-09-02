#!/bin/bash

echo "Running R REST API Client Demo..."

# Check if R is installed
if ! command -v R &> /dev/null; then
    echo "Error: R is not installed"
    echo "Please install R and required packages:"
    echo "  brew install r"
    echo "  R -e \"install.packages(c('httr', 'jsonlite'))\""
    exit 1
fi

cd client
Rscript client.R