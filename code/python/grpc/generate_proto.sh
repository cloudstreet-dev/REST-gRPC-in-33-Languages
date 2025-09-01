#!/bin/bash

# Generate Python code from proto files
python -m grpc_tools.protoc \
  -I../../shared/protos \
  --python_out=. \
  --pyi_out=. \
  --grpc_python_out=. \
  ../../shared/protos/tasks.proto

echo "Proto files generated successfully"