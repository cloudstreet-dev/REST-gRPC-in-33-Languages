# Generated Protocol Buffer Files

This directory should contain the Swift files generated from the protocol buffer definitions.

To generate the files, run:

```bash
# Install protoc if you haven't already
brew install protobuf

# Install the Swift protobuf and gRPC plugins
# Follow instructions at: https://github.com/grpc/grpc-swift

# Generate Swift files
protoc --swift_out=. --grpc-swift_out=. ../../../../shared/protos/tasks.proto
```

The generated files will include:
- `tasks.pb.swift` - Protocol buffer message definitions
- `tasks.grpc.swift` - gRPC service definitions and stubs