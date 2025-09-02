# Julia REST API Implementation

This directory contains a REST API implementation in Julia, demonstrating high-performance scientific computing with dynamic typing and multiple dispatch.

## Features

- **HTTP.jl web framework** for REST API
- **Multiple dispatch** for elegant polymorphism
- **Type system** with optional type annotations
- **High performance** with JIT compilation
- **Thread-safe operations** with locks
- **Native array operations** for data processing

## Prerequisites

- Julia 1.9 or higher

Install Julia:
```bash
# macOS
brew install julia

# Ubuntu/Debian
wget https://julialang-s3.julialang.org/bin/linux/x64/1.9/julia-1.9.4-linux-x86_64.tar.gz
tar xzf julia-1.9.4-linux-x86_64.tar.gz
export PATH=$PATH:$(pwd)/julia-1.9.4/bin

# Windows
# Download installer from https://julialang.org/downloads/

# Verify installation
julia --version
```

## Server

The server implements a complete REST API using HTTP.jl.

### Running the Server

```bash
cd server
julia -e 'using Pkg; Pkg.activate("."); Pkg.instantiate()'
julia --project=. server.jl
```

Or use the provided script:
```bash
./run-server.sh
```

The server will start on port 8080 by default.

### API Endpoints

- `GET /api/tasks` - List all tasks
- `GET /api/tasks/:id` - Get a specific task
- `POST /api/tasks` - Create a new task
- `PUT /api/tasks/:id` - Update a task
- `PATCH /api/tasks/:id/status` - Update task status
- `DELETE /api/tasks/:id` - Delete a task
- `GET /health` - Health check

### Query Parameters

- `status` - Filter by task status (pending, in-progress, completed, cancelled)
- `assigned_to` - Filter by assignee

## Client

The client provides a comprehensive SDK for interacting with the REST API.

### Running the Client Demo

```bash
cd client
julia -e 'using Pkg; Pkg.activate("."); Pkg.instantiate()'
julia --project=. client.jl
```

Or use the provided script:
```bash
./run-client.sh
```

### Using the Client Library

```julia
using TaskClient

# Create client
client = TaskClient.Client("http://localhost:8080")

# List all tasks
tasks = TaskClient.list_tasks(client)

# Create a task
task = TaskClient.create_task(
    client,
    "Learn Julia",
    description="Master scientific computing",
    priority="high",
    tags=["julia", "learning"],
    assigned_to="developer"
)

# Update task status
updated = TaskClient.update_task_status(client, task.id, "in-progress")

# Delete task
TaskClient.delete_task(client, task.id)
```

## Architecture

### Type System

```julia
# Composite types (structs)
mutable struct Task
    id::String
    title::String
    description::Union{String, Nothing}
    status::TaskStatus
    priority::TaskPriority
    tags::Vector{String}
    assigned_to::Union{String, Nothing}
    created_at::Float64
    updated_at::Float64
end

# Enums
@enum TaskStatus begin
    PENDING
    IN_PROGRESS
    COMPLETED
    CANCELLED
end

# Type unions for nullable fields
Union{String, Nothing}  # Can be String or nothing (null)
```

### Multiple Dispatch

```julia
# Define methods for different types
process(task::Task) = process_task(task)
process(tasks::Vector{Task}) = process_batch(tasks)
process(id::String) = process(get_task(id))

# Dispatch based on multiple arguments
merge(t1::Task, t2::Task) = merge_tasks(t1, t2)
merge(t::Task, updates::Dict) = apply_updates(t, updates)
merge(tasks::Vector{Task}) = reduce(merge, tasks)

# Type parameters
function filter_by_status(tasks::Vector{T}, status::S) where {T<:Task, S<:TaskStatus}
    filter(t -> t.status == status, tasks)
end
```

### Thread Safety

```julia
# ReentrantLock for thread safety
const task_lock = ReentrantLock()

function thread_safe_operation()
    lock(task_lock) do
        # Critical section
        # Lock is automatically released
    end
end

# Atomic operations
const counter = Threads.Atomic{Int}(0)
Threads.atomic_add!(counter, 1)
```

## Julia Features Demonstrated

### Metaprogramming

```julia
# Macros for code generation
macro define_getter(field)
    quote
        function $(Symbol("get_", field))(task::Task)
            return task.$field
        end
    end
end

@define_getter title  # Creates get_title(task)
@define_getter status # Creates get_status(task)

# Expression manipulation
expr = :(x + y * z)
println(expr.args)  # [:+, :x, :(y * z)]
```

### Broadcasting

```julia
# Apply operations element-wise with dot syntax
tasks = list_tasks()

# Get all titles (broadcasting)
titles = getfield.(tasks, :title)

# Update all statuses
tasks.status .= COMPLETED

# Complex operations
priorities = parse_priority.(["low", "medium", "high"])
```

### Comprehensions and Generators

```julia
# List comprehensions
completed_ids = [t.id for t in tasks if t.status == COMPLETED]

# Dictionary comprehensions
task_map = Dict(t.id => t for t in tasks)

# Generators (lazy evaluation)
high_priority = (t for t in tasks if t.priority == HIGH)

# Multi-dimensional comprehensions
matrix = [i + j for i in 1:3, j in 1:3]
```

### Performance Optimization

```julia
# Type annotations for performance
function sum_priorities(tasks::Vector{Task})::Int
    total::Int = 0
    for task in tasks
        total += Int(task.priority)
    end
    return total
end

# @inbounds for skipping bounds checking
function fast_access(arr::Vector{Float64}, indices::Vector{Int})
    result = 0.0
    @inbounds for i in indices
        result += arr[i]
    end
    return result
end

# @simd for vectorization
function vector_sum(x::Vector{Float64}, y::Vector{Float64})
    z = similar(x)
    @simd for i in eachindex(x, y)
        @inbounds z[i] = x[i] + y[i]
    end
    return z
end
```

### Package System

```julia
# Create a package
using Pkg
Pkg.generate("MyPackage")

# Add dependencies
Pkg.add("HTTP")
Pkg.add("JSON3")

# Pin specific versions
Pkg.pin("HTTP@1.10")

# Create environments
Pkg.activate("myenv")
Pkg.instantiate()
```

## Testing

### Unit Tests

```julia
using Test

@testset "Task Tests" begin
    @testset "Creation" begin
        task = create_task(CreateTaskRequest("Test", nothing, "high", nothing, nothing))
        @test task.title == "Test"
        @test task.status == PENDING
        @test task.priority == HIGH
    end
    
    @testset "Update" begin
        task = create_task(CreateTaskRequest("Test", nothing, nothing, nothing, nothing))
        updated = update_task_status(task.id, IN_PROGRESS)
        @test updated.status == IN_PROGRESS
    end
    
    @testset "Filtering" begin
        tasks = list_tasks(status_filter="pending")
        @test all(t -> t.status == PENDING, tasks)
    end
end

# Run tests
Pkg.test()
```

### Benchmarking

```julia
using BenchmarkTools

# Benchmark a function
@benchmark create_task($request)

# Compare implementations
function method1(data)
    # Implementation 1
end

function method2(data)
    # Implementation 2
end

@benchmark method1($data)
@benchmark method2($data)

# Statistical analysis
result = @benchmark list_tasks()
println("Mean time: $(mean(result.times)) ns")
println("Memory: $(result.memory) bytes")
```

## Parallel Processing

### Multi-threading

```julia
# Enable threads: julia -t 4

using Base.Threads

# Parallel for loop
Threads.@threads for task in tasks
    process_task(task)
end

# Thread-local storage
const tls = [Vector{Task}() for _ in 1:nthreads()]

# Parallel map
results = ThreadsX.map(process_task, tasks)
```

### Distributed Computing

```julia
using Distributed

# Add worker processes
addprocs(4)

@everywhere function process_task(task)
    # This function runs on all workers
    return expensive_computation(task)
end

# Parallel map across workers
results = pmap(process_task, tasks)

# Distributed for loop
@distributed for task in tasks
    save_result(process_task(task))
end
```

## Best Practices

1. **Use type annotations wisely**: For performance-critical code
2. **Leverage multiple dispatch**: Design APIs around it
3. **Avoid type instability**: Ensure functions return consistent types
4. **Use broadcasting**: More efficient than loops
5. **Profile before optimizing**: Use `@profile` and ProfileView
6. **Write type-stable code**: Use `@code_warntype` to check
7. **Use appropriate data structures**: StaticArrays for small fixed-size arrays

## Docker Support

```dockerfile
FROM julia:1.9-bullseye

WORKDIR /app

# Copy project files
COPY server/Project.toml server/Manifest.toml ./
RUN julia -e 'using Pkg; Pkg.activate("."); Pkg.instantiate()'

COPY server/ .

EXPOSE 8080
CMD ["julia", "--project=.", "server.jl"]
```

## Dependencies

### Server
- `HTTP` - Web server and client
- `JSON3` - JSON parsing with performance
- `StructTypes` - Struct serialization
- `Dates` - Date/time handling
- `UUIDs` - UUID generation

### Client
- `HTTP` - HTTP client
- `JSON3` - JSON parsing

## Advanced Features

### Custom Types with Parametric Polymorphism

```julia
# Generic container
struct Container{T}
    items::Vector{T}
    capacity::Int
end

# Methods automatically specialized
push!(c::Container{T}, item::T) where T = push!(c.items, item)

# Constraints on type parameters
struct NumericContainer{T<:Number}
    values::Vector{T}
end
```

### Lazy Evaluation

```julia
# Lazy iterators
lazy_squares = (x^2 for x in 1:1000000)

# Take only what you need
first_10 = collect(Iterators.take(lazy_squares, 10))

# Chain operations lazily
result = 1:1000000 |>
    x -> Iterators.filter(isodd, x) |>
    x -> Iterators.map(y -> y^2, x) |>
    x -> Iterators.take(x, 100) |>
    collect
```