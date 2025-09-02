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

### 🎉 All 33 Languages Complete! (v1.0.0)

#### Languages with Full gRPC Implementation (17 languages)
✅ [JavaScript](book/manuscript/chapters/01-javascript.md) (Node.js with Express and gRPC)  
✅ [TypeScript](book/manuscript/chapters/02-typescript.md) (Deno with Oak and gRPC)  
✅ [Dart](book/manuscript/chapters/03-dart.md) (Shelf framework and gRPC)  
✅ [Go](book/manuscript/chapters/04-go.md) (Gin and native gRPC)  
✅ [Ruby](book/manuscript/chapters/05-ruby.md) (Sinatra and gRPC gem)  
✅ [PHP](book/manuscript/chapters/07-php.md) (Slim Framework and gRPC)  
✅ [Python](book/manuscript/chapters/08-python.md) (FastAPI and grpcio)  
✅ [Java](book/manuscript/chapters/09-java.md) (Spring Boot and gRPC)  
✅ [Rust](book/manuscript/chapters/10-rust.md) (Actix-web and Tonic)  
✅ [Kotlin](book/manuscript/chapters/11-kotlin.md) (Ktor and gRPC)  
✅ [Swift](book/manuscript/chapters/12-swift.md) (Vapor and gRPC-Swift)  
✅ [C#](book/manuscript/chapters/13-csharp.md) (ASP.NET Core and gRPC)  
✅ [F#](book/manuscript/chapters/14-fsharp.md) (Giraffe and gRPC on .NET)  
✅ [C++](book/manuscript/chapters/15-cpp.md) (Crow and gRPC++)  
✅ [Scala](book/manuscript/chapters/16-scala.md) (Akka HTTP and ScalaPB)  
✅ [Objective-C](book/manuscript/chapters/17-objective-c.md) (GCDWebServer and gRPC-ObjC)  
✅ [Elixir](book/manuscript/chapters/21-elixir.md) (Phoenix and grpc-elixir)  
✅ [Clojure](book/manuscript/chapters/25-clojure.md) (Ring/Compojure and Protojure)  
✅ [Erlang](book/manuscript/chapters/27-erlang.md) (Cowboy and grpcbox)  
✅ [Haskell](book/manuscript/chapters/24-haskell.md) (Servant and grpc-haskell)

#### Languages with Partial gRPC Support (5 languages with examples/documentation)
✅ [Crystal](book/manuscript/chapters/06-crystal.md) (Kemal - gRPC alternatives discussed)  
✅ [Perl](book/manuscript/chapters/20-perl.md) (Mojolicious - Grpc::XS documented)  
✅ [Lua](book/manuscript/chapters/22-lua.md) (OpenResty - lua-grpc limitations shown)  
✅ [R](book/manuscript/chapters/23-r.md) (Plumber - grpc package examples)  
✅ [D](book/manuscript/chapters/19-d.md) (Vibe.d - gRPC binding challenges explained)

#### Languages with REST Only (11 languages - gRPC not feasible)
✅ [C](book/manuscript/chapters/18-c.md) (Custom HTTP server - gRPC requires C++)  
✅ [Zig](book/manuscript/chapters/26-zig.md) (Built-in HTTP - awaiting gRPC ecosystem)  
✅ [OCaml](book/manuscript/chapters/28-ocaml.md) (Dream - ocaml-grpc immature)  
✅ [Nim](book/manuscript/chapters/29-nim.md) (Jester - no production gRPC)  
✅ [Julia](book/manuscript/chapters/30-julia.md) (HTTP.jl - gRPC.jl experimental)  
✅ [Gleam](book/manuscript/chapters/31-gleam.md) (Mist - too new for gRPC)  
✅ [V](book/manuscript/chapters/32-v.md) (vweb - young language, no gRPC)  
✅ [Io](book/manuscript/chapters/33-io.md) (Socket-based - niche language, gRPC infeasible)

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

## Authors

**David Christian Liedle**

- Email: david.liedle@gmail.com
- GitHub: [@davidliedle](https://github.com/davidliedle)

**With AI Co-authorship by Claude (Anthropic)**

This book represents a collaboration between human expertise and AI assistance, with Claude helping to implement code examples, write documentation, and ensure consistency across all 33 language implementations.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Acknowledgments

- The open-source community for the amazing tools and libraries
- My children, Tegan and Solomon, for their patience and inspiration
- All who have supported this ambitious project

## Project Status

**Version 1.0.0 Complete!** 🎉

All 33 language implementations are complete with comprehensive REST API examples and gRPC implementations or documentation where applicable.

### Completed Milestones

- [x] Project structure and shared specifications
- [x] All 33 programming language implementations
- [x] REST API implementation for all languages
- [x] gRPC implementation for 22 languages with mature support
- [x] gRPC considerations documented for 11 languages with limitations
- [x] Complete manuscript with all 33 chapters
- [x] Comprehensive documentation and code examples
- [x] Performance comparisons and best practices
- [x] Tagged v1.0.0 release

### Future Enhancements

- [ ] Additional shell/scripting language supplements
- [ ] Enhanced performance benchmarks
- [ ] Video tutorials and walkthroughs
- [ ] Interactive online playground
- [ ] Community contributions and optimizations
- [ ] Publisher review for print edition
- [ ] Translations to other languages

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