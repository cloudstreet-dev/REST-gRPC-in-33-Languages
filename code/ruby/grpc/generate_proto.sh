#!/bin/bash

# Generate Ruby code from proto files
grpc_tools_ruby_protoc \
  -I../../shared/protos \
  --ruby_out=./lib \
  --grpc_out=./lib \
  ../../shared/protos/tasks.proto

echo "Proto files generated successfully"