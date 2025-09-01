#!/bin/bash

# Generate PHP code from proto files
protoc \
  --proto_path=../../shared/protos \
  --php_out=./generated \
  --grpc_out=./generated \
  --plugin=protoc-gen-grpc=/usr/local/bin/grpc_php_plugin \
  ../../shared/protos/tasks.proto

echo "Proto files generated successfully"