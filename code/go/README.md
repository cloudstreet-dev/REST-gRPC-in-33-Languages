# Go Implementation

This directory contains both REST and gRPC implementations of the Task Management API in Go.

## Directory Structure

```
go/
├── cmd/                    # Application entry points
│   ├── rest-server/       # REST server main
│   ├── rest-client/       # REST client main
│   ├── grpc-server/       # gRPC server main
│   └── grpc-client/       # gRPC client main
├── internal/              # Private application code
│   ├── models/           # Data models
│   └── services/         # Business logic
├── rest/                  # REST-specific code
│   ├── server/           # REST server handlers
│   └── client/           # REST client implementation
├── grpc/                  # gRPC-specific code
│   ├── server/           # gRPC server implementation
│   ├── client/           # gRPC client implementation
│   └── generate.sh       # Proto generation script
└── go.mod                 # Module definition

```

## Prerequisites

- Go 1.21 or higher
- Protocol Buffers compiler (protoc) for gRPC code generation
- Go plugins for protoc:
  ```bash
  go install google.golang.org/protobuf/cmd/protoc-gen-go@latest
  go install google.golang.org/grpc/cmd/protoc-gen-go-grpc@latest
  ```

## Installation

```bash
cd code/go
go mod download
```

## REST API

### Server Setup

```bash
go run cmd/rest-server/main.go
# Or build and run
go build -o bin/rest-server cmd/rest-server/main.go
./bin/rest-server
```

The REST API server will run on `http://localhost:8080`

### REST Client Usage

```bash
go run cmd/rest-client/main.go
```

### REST API Endpoints

- `GET /health` - Health check
- `GET /api/v1/tasks` - List all tasks
- `GET /api/v1/tasks/:id` - Get a specific task
- `POST /api/v1/tasks` - Create a new task
- `PUT /api/v1/tasks/:id` - Update a task
- `PATCH /api/v1/tasks/:id/status` - Update task status
- `DELETE /api/v1/tasks/:id` - Delete a task

## gRPC API

### Generate Proto Files

```bash
cd grpc
./generate.sh
```

### Server Setup

```bash
go run cmd/grpc-server/main.go
# Or build and run
go build -o bin/grpc-server cmd/grpc-server/main.go
./bin/grpc-server
```

The gRPC server will run on `localhost:50051`

### gRPC Client Usage

```bash
go run cmd/grpc-client/main.go
```

### gRPC Service Methods

- `ListTasks` - Server streaming method to list tasks
- `GetTask` - Unary call to get a single task
- `CreateTask` - Unary call to create a task
- `UpdateTask` - Unary call to update a task
- `DeleteTask` - Unary call to delete a task
- `WatchTasks` - Bidirectional streaming for real-time updates

## Testing

### Run Tests

```bash
go test ./...
go test -v ./... # Verbose output
go test -cover ./... # With coverage
```

### Test Both APIs

```bash
# Terminal 1 - Start REST server
go run cmd/rest-server/main.go

# Terminal 2 - Start gRPC server
go run cmd/grpc-server/main.go

# Terminal 3 - Run clients
go run cmd/rest-client/main.go
go run cmd/grpc-client/main.go
```

## Building

### Build All Binaries

```bash
# Create bin directory
mkdir -p bin

# Build servers
go build -o bin/rest-server cmd/rest-server/main.go
go build -o bin/grpc-server cmd/grpc-server/main.go

# Build clients
go build -o bin/rest-client cmd/rest-client/main.go
go build -o bin/grpc-client cmd/grpc-client/main.go
```

### Cross-Compilation

```bash
# Linux
GOOS=linux GOARCH=amd64 go build -o bin/rest-server-linux cmd/rest-server/main.go

# Windows
GOOS=windows GOARCH=amd64 go build -o bin/rest-server.exe cmd/rest-server/main.go

# macOS ARM64
GOOS=darwin GOARCH=arm64 go build -o bin/rest-server-mac cmd/rest-server/main.go
```

## Docker Deployment

```dockerfile
# Dockerfile
FROM golang:1.21-alpine AS builder

WORKDIR /app
COPY go.* ./
RUN go mod download

COPY . .
RUN go build -o rest-server cmd/rest-server/main.go
RUN go build -o grpc-server cmd/grpc-server/main.go

FROM alpine:latest
RUN apk --no-cache add ca-certificates
WORKDIR /root/

COPY --from=builder /app/rest-server .
COPY --from=builder /app/grpc-server .

EXPOSE 8080 50051

CMD ["./rest-server"]
```

## Features Demonstrated

### REST Implementation
- Gin framework for HTTP routing
- Middleware pipeline (CORS, logging, recovery)
- Request validation with struct tags
- JSON serialization
- Error handling with proper HTTP status codes
- Pagination and filtering

### gRPC Implementation
- Native Go gRPC support
- Protocol Buffers v3
- Server streaming (ListTasks)
- Unary calls (Get, Create, Update, Delete)
- Bidirectional streaming (WatchTasks)
- Error handling with gRPC status codes
- Reflection for grpcurl support

## Performance Considerations

Go's implementation typically shows:
- Excellent concurrency with goroutines
- Low memory footprint
- Fast compilation times
- Native gRPC performance
- Efficient garbage collection

## Common Issues

### Port Already in Use
```bash
# Find process using port
lsof -i :8080
lsof -i :50051

# Kill the process
kill -9 <PID>
```

### Proto Generation Issues
Ensure protoc and Go plugins are installed:
```bash
# Install protoc (macOS)
brew install protobuf

# Install Go plugins
go install google.golang.org/protobuf/cmd/protoc-gen-go@latest
go install google.golang.org/grpc/cmd/protoc-gen-go-grpc@latest

# Add to PATH
export PATH="$PATH:$(go env GOPATH)/bin"
```

### Module Issues
```bash
# Clean module cache
go clean -modcache

# Update dependencies
go get -u ./...

# Tidy modules
go mod tidy
```

## Next Steps

- Add database persistence (PostgreSQL, MongoDB)
- Implement authentication (JWT, OAuth2)
- Add comprehensive test coverage
- Set up CI/CD pipeline
- Add metrics and tracing (Prometheus, OpenTelemetry)
- Implement rate limiting
- Add WebSocket support for real-time updates