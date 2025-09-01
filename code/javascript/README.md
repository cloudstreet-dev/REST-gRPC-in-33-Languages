# JavaScript Implementation

This directory contains both REST and gRPC implementations of the Task Management API in JavaScript using Node.js.

## Directory Structure

```
javascript/
├── rest/
│   ├── server/      # Express.js REST API server
│   └── client/      # REST API client
└── grpc/
    ├── server/      # gRPC server implementation
    └── client/      # gRPC client implementation
```

## Prerequisites

- Node.js 18.0 or higher
- npm 8.0 or higher

## REST API

### Server Setup

```bash
cd rest/server
npm install
npm start       # Production mode
npm run dev     # Development mode with auto-reload
```

The REST API server will run on `http://localhost:8080`

### REST Client Usage

```bash
cd rest/client
node client.js
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

### Server Setup

```bash
cd grpc/server
npm install
npm start       # Production mode
npm run dev     # Development mode with auto-reload
```

The gRPC server will run on `localhost:50051`

### gRPC Client Usage

```bash
cd grpc/client
npm install
npm start
```

### gRPC Service Methods

- `ListTasks` - Server streaming method to list tasks
- `GetTask` - Unary call to get a single task
- `CreateTask` - Unary call to create a task
- `UpdateTask` - Unary call to update a task
- `DeleteTask` - Unary call to delete a task
- `WatchTasks` - Bidirectional streaming for real-time updates

## Testing

### Test Both APIs

```bash
# Terminal 1 - Start REST server
cd rest/server && npm start

# Terminal 2 - Start gRPC server
cd grpc/server && npm start

# Terminal 3 - Run tests
cd rest/client && node client.js
cd grpc/client && node client.js
```

## Docker Deployment

```bash
# Build the image
docker build -t task-api-js .

# Run the container
docker run -p 8080:8080 -p 50051:50051 task-api-js
```

## Features Demonstrated

### REST Implementation
- Express.js middleware pipeline
- Request validation
- Error handling middleware
- CORS configuration
- Security headers with Helmet
- Request logging with Morgan
- Async/await patterns
- RESTful resource design

### gRPC Implementation
- Protocol Buffers integration
- Server streaming (ListTasks)
- Unary calls (Get, Create, Update, Delete)
- Bidirectional streaming (WatchTasks)
- Error handling with gRPC status codes
- Real-time event notifications

## Performance Comparison

The gRPC implementation typically shows:
- 30-40% lower latency for simple operations
- 50-60% smaller payload sizes
- Better performance for streaming operations
- Type safety through Protocol Buffers

## Security Notes

In production, you should:
1. Enable TLS/SSL for both REST and gRPC
2. Implement proper authentication (JWT, OAuth2)
3. Add rate limiting
4. Validate and sanitize all inputs
5. Use environment variables for configuration
6. Implement proper logging and monitoring

## Common Issues

### Port Already in Use
```bash
# Find process using port 8080
lsof -i :8080
# Kill the process
kill -9 <PID>
```

### Module Resolution Issues
Ensure you're using Node.js 18+ which has better ESM support:
```bash
node --version  # Should be v18.0.0 or higher
```

### gRPC Connection Issues
Check that the proto file path is correct and the gRPC server is running:
```bash
# Test gRPC server with grpcurl
grpcurl -plaintext localhost:50051 list
```

## Next Steps

- Add database persistence (MongoDB, PostgreSQL)
- Implement authentication and authorization
- Add comprehensive test suite
- Set up CI/CD pipeline
- Add OpenTelemetry for observability
- Implement caching layer
- Add WebSocket support for REST API real-time updates