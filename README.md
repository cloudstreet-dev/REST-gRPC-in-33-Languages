# REST APIs and gRPC in 33 Languages

A comprehensive guide and working examples of REST and gRPC API implementations across 33 programming languages.

## About This Project

This repository contains the complete source code and manuscript for the book "REST APIs and gRPC in 33 Languages: A Polyglot's Guide to Modern API Creation, Implementation, and Usage" by David Christian Liedle.

The project demonstrates how to implement the same Task Management API using both REST and gRPC protocols in 33 different programming languages, providing a unique comparative perspective on API development across the modern programming landscape.

## Project Structure

```
REST-gRPC-in-33-Languages/
├── book/                   # Book manuscript and documentation
│   ├── manuscript/        # Chapter content in Markdown
│   │   ├── frontmatter/  # Foreword, preface, acknowledgments
│   │   ├── chapters/     # 33 language chapters
│   │   └── appendices/   # Installation guides, tools, etc.
│   └── resources/        # Images, diagrams, references
├── code/                  # Working code examples
│   ├── shared/           # Shared protocol definitions
│   │   ├── protos/      # Protocol Buffer definitions
│   │   └── openapi.yaml # OpenAPI 3.0 specification
│   └── [language]/      # Per-language implementations
│       ├── rest/        # REST API implementation
│       │   ├── server/
│       │   └── client/
│       └── grpc/        # gRPC implementation
│           ├── server/
│           └── client/
└── LICENSE

```

## The Task Management API

All implementations provide the same Task Management API functionality:

### REST Endpoints
- `GET /tasks` - List all tasks with filtering and pagination
- `GET /tasks/{id}` - Get a specific task
- `POST /tasks` - Create a new task
- `PUT /tasks/{id}` - Update a task
- `PATCH /tasks/{id}/status` - Update task status
- `DELETE /tasks/{id}` - Delete a task

### gRPC Services
- `ListTasks` - Server streaming: list tasks with filters
- `GetTask` - Unary: get a single task
- `CreateTask` - Unary: create a new task
- `UpdateTask` - Unary: update an existing task
- `DeleteTask` - Unary: delete a task
- `WatchTasks` - Bidirectional streaming: real-time task updates

## Languages Covered

### Currently Implemented
✅ [JavaScript](book/manuscript/chapters/01-javascript.md) (Node.js with Express and gRPC)  
✅ [TypeScript](book/manuscript/chapters/02-typescript.md) (Deno with Oak and gRPC)  
✅ [Dart](book/manuscript/chapters/03-dart.md) (Shelf framework and simplified gRPC)  
✅ [Go](book/manuscript/chapters/04-go.md) (Gin and native gRPC)  
✅ [Ruby](book/manuscript/chapters/05-ruby.md) (Sinatra and gRPC)  
✅ [Crystal](book/manuscript/chapters/06-crystal.md) (Kemal and gRPC alternatives)  
✅ [PHP](book/manuscript/chapters/07-php.md) (Slim Framework and gRPC)  
✅ [Python](book/manuscript/chapters/08-python.md) (FastAPI and gRPC)  
✅ [Java](book/manuscript/chapters/09-java.md) (Spring Boot and gRPC)  
✅ [Rust](book/manuscript/chapters/10-rust.md) (Actix-web and Tonic)  
✅ [Kotlin](book/manuscript/chapters/11-kotlin.md) (Ktor and gRPC)  
✅ [Swift](book/manuscript/chapters/12-swift.md) (Vapor and gRPC)  
✅ [C#](book/manuscript/chapters/13-csharp.md) (ASP.NET Core and gRPC)  
✅ [F#](book/manuscript/chapters/14-fsharp.md) (ASP.NET Core and gRPC)  
✅ [C++](book/manuscript/chapters/15-cpp.md) (Crow and gRPC++)  
✅ [Scala](book/manuscript/chapters/16-scala.md) (Akka HTTP and ScalaPB)  
✅ [Objective-C](book/manuscript/chapters/17-objective-c.md) (Foundation and GCDWebServer)  
✅ [C](book/manuscript/chapters/18-c.md) (Custom HTTP server)  
✅ [D](book/manuscript/chapters/19-d.md) (Vibe.d)  
✅ [Perl](book/manuscript/chapters/20-perl.md) (Mojolicious)  
✅ [Elixir](book/manuscript/chapters/21-elixir.md) (Phoenix and gRPC)  
✅ [Lua](book/manuscript/chapters/22-lua.md) (OpenResty)  
✅ [R](book/manuscript/chapters/23-r.md) (Plumber)  
✅ [Haskell](book/manuscript/chapters/24-haskell.md) (Servant)  
✅ [Clojure](book/manuscript/chapters/25-clojure.md) (Ring and Compojure)  
✅ [Zig](book/manuscript/chapters/26-zig.md) (Built-in HTTP server)  
✅ [Erlang](book/manuscript/chapters/27-erlang.md) (Cowboy and OTP)  
✅ [OCaml](book/manuscript/chapters/28-ocaml.md) (Dream framework)

### Planned (5 languages remaining)

#### Functional & Specialized
- Nim (Jester)

#### Emerging & Modern
- Julia (HTTP.jl)
- Gleam (Mist)
- V (vweb)
- Io (Socket-based)

## Getting Started

### Prerequisites
- Docker (recommended for consistent environments)
- Or native development tools for each language

### Quick Start with JavaScript

```bash
# Clone the repository
git clone https://github.com/cloudstreet-dev/REST-gRPC-in-33-Languages.git
cd REST-gRPC-in-33-Languages

# Start JavaScript REST server
cd code/javascript/rest/server
npm install
npm start

# In another terminal, start gRPC server
cd code/javascript/grpc/server
npm install
npm start

# Test with clients
cd code/javascript/rest/client
node client.js

cd code/javascript/grpc/client
node client.js
```

### Using Docker

Each language implementation includes a Dockerfile:

```bash
cd code/javascript
docker build -t task-api-js .
docker run -p 8080:8080 -p 50051:50051 task-api-js
```

## Book Contents

### Part 1: Foundations
- Introduction to REST and gRPC
- Protocol Buffers vs JSON
- API Design Principles
- The Task Management Domain

### Part 2: Language Implementations
33 chapters, each covering:
- Language overview and philosophy
- REST API implementation
- gRPC implementation
- Performance comparisons
- Best practices and patterns
- Quick reference guide

### Part 3: Appendices
- Installation guides for all languages
- Development environment setup
- API testing tools
- Enterprise considerations
- Framework comparisons

## Features Demonstrated

Each implementation showcases:

### REST Features
- RESTful resource design
- HTTP status codes
- Request/response headers
- Content negotiation
- CORS handling
- Authentication patterns
- Error handling
- Pagination
- Filtering and sorting

### gRPC Features
- Protocol Buffer definitions
- Unary RPC calls
- Server streaming
- Client streaming
- Bidirectional streaming
- Error handling with status codes
- Metadata handling
- Interceptors/middleware
- Load balancing patterns

## Performance Comparisons

Each chapter includes benchmarks comparing:
- Latency (REST vs gRPC)
- Throughput
- Payload size
- CPU usage
- Memory consumption
- Streaming performance

## Contributing

Contributions are welcome! Please see [CONTRIBUTING.md](CONTRIBUTING.md) for guidelines.

Areas where help is needed:
- Additional language implementations
- Performance optimizations
- Security enhancements
- Documentation improvements
- Test coverage
- Docker optimizations

## Author

**David Christian Liedle**

- Email: david.liedle@gmail.com
- GitHub: [@davidliedle](https://github.com/davidliedle)

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Acknowledgments

- The open-source community for the amazing tools and libraries
- My children, Tegan and Solomon, for their patience and inspiration
- All who have supported this ambitious project

## Project Status

This is an active project under development. The book and code examples are being created incrementally, with new language implementations added regularly.

### Roadmap

- [x] Project structure and shared specifications
- [x] JavaScript implementation
- [x] TypeScript implementation
- [x] Dart implementation
- [x] Go implementation
- [x] Ruby implementation
- [x] Crystal implementation
- [x] PHP implementation
- [x] Python implementation
- [x] Java implementation
- [x] Rust implementation
- [x] Kotlin implementation
- [x] Swift implementation
- [x] C# implementation
- [x] F# implementation
- [ ] 6 more mainstream languages
- [ ] 10 functional/specialized languages
- [ ] 3 shell/scripting languages
- [ ] Complete manuscript
- [ ] Publisher review
- [ ] First edition release

## Resources

### Documentation
- [OpenAPI Specification](code/shared/openapi.yaml)
- [Protocol Buffer Definition](code/shared/protos/tasks.proto)
- [JavaScript Implementation](code/javascript/README.md)

### External Links
- [REST API Design Best Practices](https://restfulapi.net/)
- [gRPC Official Documentation](https://grpc.io/)
- [Protocol Buffers Guide](https://developers.google.com/protocol-buffers)

## Support

For questions, issues, or discussions:
- Open an issue on GitHub
- Email the author
- Join the discussion in the repository's Discussions tab

---

*"The best way to learn is to teach, and the best code is written to meet one's own needs."*

This book is a labor of love, created to be the reference I wish I had on my desk.