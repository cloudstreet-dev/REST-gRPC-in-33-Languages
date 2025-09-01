# Introduction

REST APIs dominate the landscape at the time of writing. gRPC is new on the scene but deserves attention. Bridging the gap between the two requires a bit of history and understanding.

## The Evolution of APIs

In the beginning, there was RPC (Remote Procedure Call). The idea was simple: make calling a function on a remote computer as easy as calling a local function. From Sun RPC in the 1980s to CORBA in the 1990s to SOAP in the early 2000s, we've been trying to abstract away the network.

Then came REST (Representational State Transfer), introduced by Roy Fielding in his 2000 doctoral dissertation. REST wasn't trying to hide the network—it embraced it. By leveraging HTTP's existing methods (GET, POST, PUT, DELETE) and status codes, REST APIs felt natural to web developers. The simplicity was revolutionary. No complex tooling, no code generation, just HTTP.

REST's dominance from 2005 to today stems from several factors:
- **Simplicity**: Any language that can make HTTP requests can consume a REST API
- **Tooling**: Browsers, curl, Postman—debugging tools already existed
- **Statelessness**: Each request contains all necessary information
- **Caching**: HTTP's caching mechanisms work out of the box
- **Readability**: JSON payloads are human-readable

But REST has limitations:
- **Over/Under Fetching**: Getting exactly the data you need requires careful API design
- **Multiple Round Trips**: Complex operations often require multiple requests
- **No Type Safety**: JSON doesn't enforce schemas without additional tooling
- **Streaming**: REST wasn't designed for real-time, bidirectional communication
- **Performance**: Text-based JSON is larger than binary formats

Enter gRPC in 2015, Google's open-source RPC framework. gRPC addresses REST's limitations while introducing its own trade-offs:
- **Protocol Buffers**: Binary format with schema definition and code generation
- **HTTP/2**: Multiplexing, server push, header compression
- **Streaming**: First-class support for server, client, and bidirectional streaming
- **Type Safety**: Strong typing across language boundaries
- **Performance**: Lower latency, smaller payloads

## Why Both?

This book covers both REST and gRPC because modern systems need both. REST excels for:
- Public APIs (browsers can't speak gRPC directly)
- Simple CRUD operations
- Systems where human readability matters
- Loose coupling between services
- Maximum compatibility

gRPC excels for:
- Internal microservices communication
- Real-time systems requiring streaming
- Performance-critical applications
- Systems requiring strong type safety
- Polyglot environments with code generation

## The Task Management Domain

We've chosen a Task Management API as our example domain for several reasons:

1. **Universal Understanding**: Everyone understands tasks, priorities, and deadlines
2. **Rich Enough**: It provides enough complexity to showcase different features
3. **Simple Enough**: It won't obscure the API patterns we're demonstrating
4. **Practical**: It's a real system you might actually build
5. **Extensible**: Easy to add features like comments, attachments, or notifications

Our Task model includes:
- **Identification**: Unique IDs for each task
- **Content**: Title and description
- **Status**: Pending, in-progress, completed, etc.
- **Priority**: Low, medium, high, critical
- **Organization**: Tags for categorization
- **Assignment**: User assignment and creation tracking
- **Timing**: Creation, update, due, and completion timestamps

This model is rich enough to demonstrate:
- CRUD operations (Create, Read, Update, Delete)
- Filtering and searching
- Pagination
- Sorting
- Real-time updates (via streaming)
- State transitions
- Validation

## What Makes This Book Different

There are many books about REST APIs. There are growing numbers of books about gRPC. There are books about specific programming languages. What makes this book unique is its truly polyglot nature.

Most API books show examples in one or two languages, leaving you to figure out how to apply the concepts in your language of choice. This book implements the exact same API in 33 different languages, allowing you to:

1. **See Patterns**: Identify patterns that transcend language boundaries
2. **Compare Approaches**: Understand how different languages solve the same problems
3. **Learn Idioms**: See idiomatic code for each language
4. **Make Decisions**: Compare performance, readability, and ecosystem
5. **Bridge Gaps**: Understand how to make different languages work together

## Structure of Each Chapter

Every language chapter follows the same structure:

1. **About the Language**: History, philosophy, and use cases
2. **Ecosystem Overview**: Package management, popular frameworks, tooling
3. **REST Implementation**: Complete server and client
4. **gRPC Implementation**: Complete server and client  
5. **Testing**: How to verify the implementation works
6. **Performance**: Benchmarks comparing REST and gRPC
7. **Best Practices**: Language-specific patterns and anti-patterns
8. **Quick Reference**: Common commands and snippets

This consistency makes it easy to jump between languages and compare implementations.

## How to Read This Book

This book is designed for multiple reading patterns:

### The Completionist Path
Read cover to cover to gain a comprehensive understanding of the modern programming landscape. You'll see the evolution from older languages like C to modern ones like Rust, from dynamic languages like Python to statically typed ones like Haskell.

### The Practical Path
Jump directly to the languages you use. If you're a full-stack JavaScript developer working with Python backends, read those chapters first. Use others as reference when needed.

### The Comparative Path
Pick a feature (like streaming or error handling) and read how each language implements it. This approach helps you understand different paradigms and philosophies.

### The Migration Path
If you're moving from one language to another, read both chapters side by side. Understanding how familiar concepts translate helps accelerate learning.

### The Architecture Path
Focus on the performance comparisons and best practices sections to make informed decisions about language choice for different components of your system.

## What You'll Build

By the end of any single chapter, you'll have:
- A working REST API server with full CRUD operations
- A REST API client demonstrating all operations
- A gRPC server with unary and streaming methods
- A gRPC client demonstrating all RPC types
- Comprehensive tests
- Performance benchmarks
- Docker configuration for deployment

Multiply that by 33 languages, and you'll have a comprehensive reference implementation library.

## Prerequisites

Each chapter assumes basic familiarity with programming concepts but not with the specific language. We provide:
- Installation instructions for each language
- Package management setup
- IDE/editor recommendations
- Testing framework setup
- Debugging tool suggestions

The code is designed to be educational—clear and well-commented rather than cleverly concise.

## A Word on Opinion

This book contains opinions. When there are multiple ways to accomplish something, we've chosen one and explained why. These choices are based on:
- Community best practices
- Production experience
- Clarity for learning
- Consistency across chapters

You may disagree with some choices, and that's fine. The goal is to provide working, understandable implementations that demonstrate core concepts.

## Let's Begin

With 33 languages ahead of us, we have a lot of ground to cover. Each language tells a story—why it was created, what problems it solves, and how it thinks about the world. By implementing the same API in all of them, we'll see these stories unfold in code.

Whether you're here to learn a new language, understand API design, or simply satisfy your curiosity about how different languages approach the same problem, welcome. Let's build some APIs and learn together.

The journey starts with JavaScript—the language of the web and, increasingly, everything else. From there, we'll travel through typed and untyped languages, compiled and interpreted, functional and object-oriented, vintage and cutting-edge.

Ready? Let's code.