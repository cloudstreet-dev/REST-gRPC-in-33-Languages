#!/bin/bash

# Generate Go code from proto files
echo "Generating Go gRPC code..."

# Ensure protoc is installed
if ! command -v protoc &> /dev/null; then
    echo "protoc is not installed. Please install protocol buffers compiler."
    exit 1
fi

# Generate Go code
protoc \
    --go_out=. \
    --go_opt=paths=source_relative \
    --go-grpc_out=. \
    --go-grpc_opt=paths=source_relative \
    tasks.proto

echo "✅ Go gRPC code generation complete!"
echo "Generated files:"
echo "  - tasks.pb.go (Protocol Buffer messages)"
echo "  - tasks_grpc.pb.go (gRPC service definitions)"

# Note: To install required tools:
# go install google.golang.org/protobuf/cmd/protoc-gen-go@latest
# go install google.golang.org/grpc/cmd/protoc-gen-go-grpc@latest