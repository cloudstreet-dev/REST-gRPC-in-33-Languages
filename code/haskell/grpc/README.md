# Haskell gRPC Implementation

This directory contains a gRPC implementation of the Task Management API in Haskell using the `grpc-haskell` library.

## Overview

Haskell has excellent gRPC support through the `grpc-haskell` library, which provides:
- Type-safe service definitions
- Protocol Buffer integration
- All streaming modes (unary, server, client, bidirectional)
- High performance with low-level control

## Prerequisites

- GHC 8.10 or later
- Stack or Cabal
- Protocol Buffer compiler (protoc)
- gRPC C++ library

## Installation

### Install gRPC C++ library

```bash
# macOS
brew install grpc protobuf

# Ubuntu/Debian
sudo apt-get install libgrpc++-dev protobuf-compiler-grpc

# From source
git clone --recurse-submodules https://github.com/grpc/grpc
cd grpc
mkdir -p cmake/build
cd cmake/build
cmake ../..
make
sudo make install
```

### Install Haskell dependencies

```bash
# Using Stack
stack setup
stack build

# Using Cabal
cabal update
cabal install --dependencies-only
```

## Project Structure

```
grpc/
├── server/
│   ├── src/
│   │   └── Main.hs          # Server implementation
│   └── package.yaml          # Server dependencies
├── client/
│   ├── src/
│   │   └── Main.hs          # Client implementation
│   └── package.yaml          # Client dependencies
└── README.md                 # This file
```

## Protocol Buffer Definition

In a production setup, you would generate Haskell code from the proto file:

```bash
# Generate Haskell code from proto
compile-proto-file --proto task.proto --out src
```

The generated code would provide:
- Message types with lenses
- Service definitions
- Client stubs
- Server handlers

## Running the Server

```bash
cd server
stack run
# Or
cabal run task-grpc-server
```

The server will start on port 50051.

## Running the Client

```bash
cd client

# List all tasks
stack run -- list

# List tasks by status
stack run -- list pending

# Create a new task
stack run -- create "Learn Haskell" "Master functional programming"
```

## Implementation Notes

### Type Safety

Haskell's type system ensures compile-time safety for:
- Message field types
- RPC method signatures
- Streaming modes
- Error handling

### Performance

The implementation uses:
- STM for thread-safe state management
- Lazy evaluation for efficient streaming
- Zero-copy message passing where possible

### Production Considerations

For production use:
1. Generate code from `.proto` files for type safety
2. Implement proper error handling with status codes
3. Add TLS/SSL for secure communication
4. Use connection pooling for clients
5. Implement health checks and monitoring
6. Add distributed tracing support

## Testing

```haskell
-- Property-based testing with QuickCheck
import Test.QuickCheck

prop_roundtrip :: Task -> Bool
prop_roundtrip task = 
    decode (encode task) == Right task

-- Integration testing
spec :: Spec
spec = describe "TaskService" $ do
    it "creates and retrieves tasks" $ do
        client <- createClient defaultConfig
        task <- createTask client "Test" "Description"
        retrieved <- getTask client (taskId task)
        retrieved `shouldBe` Just task
```

## Benchmarking

```bash
# Run benchmarks
stack bench
```

## Alternative Libraries

- `mu-grpc`: Modern gRPC implementation with better ergonomics
- `grpc-native`: Lower-level bindings for more control
- `http2-grpc-haskell`: Pure Haskell implementation

## Resources

- [grpc-haskell Documentation](https://github.com/grpc/grpc-haskell)
- [Protocol Buffers for Haskell](https://github.com/awakesecurity/proto3-suite)
- [Haskell gRPC Tutorial](https://github.com/grpc/grpc-haskell/tree/master/examples)