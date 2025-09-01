#!/bin/bash

# Script to generate Dart code from proto files

# Ensure protoc is installed
if ! command -v protoc &> /dev/null; then
    echo "protoc is not installed. Please install protocol buffers compiler."
    exit 1
fi

# Ensure Dart protoc plugin is installed
if ! command -v protoc-gen-dart &> /dev/null; then
    echo "Installing Dart protoc plugin..."
    dart pub global activate protoc_plugin
fi

# Create output directories
mkdir -p server/lib/generated
mkdir -p client/lib/generated

# Generate Dart code from proto files
echo "Generating Dart gRPC code..."
protoc \
    --dart_out=grpc:server/lib/generated \
    --proto_path=../../shared/protos \
    ../../shared/protos/tasks.proto

# Copy generated files to client
cp -r server/lib/generated/* client/lib/generated/

echo "✅ Dart gRPC code generation complete!"
echo "Generated files in:"
echo "  - server/lib/generated/"
echo "  - client/lib/generated/"