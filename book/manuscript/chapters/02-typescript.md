# Chapter 2: TypeScript

## About the TypeScript Programming Language

TypeScript, created by Microsoft and first released in 2012, represents one of the most successful attempts to add static typing to a dynamic language. Anders Hejlsberg, the lead architect of C# and creator of Turbo Pascal, designed TypeScript to address the challenges of building large-scale JavaScript applications while maintaining full compatibility with existing JavaScript code.

The genius of TypeScript lies not in replacing JavaScript but in augmenting it. Every valid JavaScript program is also a valid TypeScript program, making adoption incremental and reversible. This "JavaScript with types" approach has proven so successful that TypeScript has become the de facto standard for large JavaScript projects, with adoption by major frameworks like Angular, Vue 3, and many React projects.

TypeScript's type system is remarkably sophisticated, featuring:
- **Structural typing**: Types are compatible based on their structure, not their declaration
- **Type inference**: The compiler deduces types when not explicitly declared
- **Union and intersection types**: Combining types in flexible ways
- **Generics**: Writing reusable, type-safe code
- **Mapped and conditional types**: Creating types based on other types
- **Decorators**: Adding metadata and modifying behavior (experimental)

The language has evolved rapidly, with regular releases adding features like optional chaining, nullish coalescing, template literal types, and sophisticated type manipulation capabilities that push the boundaries of what's possible in a type system.

## TypeScript's Relationship with JavaScript

TypeScript is a strict superset of JavaScript, meaning it adds features but doesn't remove any. This relationship is fundamental to TypeScript's design philosophy and success:

1. **Gradual adoption**: You can rename a `.js` file to `.ts` and start adding types incrementally
2. **JavaScript ecosystem**: All JavaScript libraries work in TypeScript (with or without type definitions)
3. **Compilation target**: TypeScript compiles to readable JavaScript, supporting various ECMAScript versions
4. **Type erasure**: Types exist only at compile time; the runtime is pure JavaScript

This relationship creates interesting dynamics:
- TypeScript often gets JavaScript features before they're standardized (decorators, private fields)
- The TypeScript compiler serves as a powerful JavaScript transpiler
- Type definition files (`.d.ts`) can describe JavaScript libraries without modifying them
- The community maintains DefinitelyTyped, a massive repository of type definitions

## TypeScript as a Server (Deno)

While Node.js requires a build step to run TypeScript, Deno—created by Node.js's original author Ryan Dahl—runs TypeScript natively. Deno represents a ground-up rethinking of server-side JavaScript/TypeScript with several key improvements:

1. **First-class TypeScript support**: No configuration or build step required
2. **Security by default**: Explicit permissions for file, network, and environment access
3. **Standard library**: Maintained and reviewed standard modules
4. **ES modules only**: No require() or CommonJS complexity
5. **Built-in tooling**: Formatter, linter, test runner, and bundler included
6. **Web API compatibility**: Uses Web Platform APIs where possible

For our REST and gRPC implementations, we'll use Deno to showcase TypeScript without the complexity of build tools, while also demonstrating modern JavaScript runtime capabilities.

## TypeScript as a Client (Angular and Beyond)

TypeScript has become the default choice for many frontend frameworks:

- **Angular**: Built with TypeScript from the ground up, making full use of decorators and dependency injection
- **Vue 3**: Rewritten in TypeScript with excellent type support
- **React**: While not requiring TypeScript, it has first-class support
- **Svelte**: Offers TypeScript support with type-safe components

The benefits in frontend development are substantial:
- IDE support with autocomplete and refactoring
- Catching errors at compile time rather than runtime
- Self-documenting code through type annotations
- Better collaboration in large teams
- Confident refactoring of complex applications

## REST API Implementation

Let's implement our Task Management REST API using TypeScript and Deno with the Oak framework (Deno's equivalent to Express).

### Project Setup

First, let's create our project structure and configuration:

```typescript
// code/typescript/rest/server/deno.json
{
  "tasks": {
    "dev": "deno run --watch --allow-net --allow-env --allow-read main.ts",
    "start": "deno run --allow-net --allow-env --allow-read main.ts",
    "test": "deno test --allow-net --allow-env --allow-read",
    "fmt": "deno fmt",
    "lint": "deno lint"
  },
  "imports": {
    "oak": "https://deno.land/x/oak@v12.6.0/mod.ts",
    "uuid": "https://deno.land/std@0.203.0/uuid/mod.ts",
    "cors": "https://deno.land/x/cors@v1.2.2/mod.ts"
  },
  "compilerOptions": {
    "strict": true,
    "noImplicitAny": true,
    "strictNullChecks": true,
    "strictFunctionTypes": true,
    "strictBindCallApply": true,
    "strictPropertyInitialization": true,
    "noImplicitThis": true,
    "alwaysStrict": true
  }
}
```

### Type Definitions

```typescript
// code/typescript/rest/server/types/task.types.ts
export interface Task {
  id: string;
  title: string;
  description: string;
  status: TaskStatus;
  priority: TaskPriority;
  tags: string[];
  created_by: string;
  assigned_to: string | null;
  created_at: string;
  updated_at: string;
  due_date: string | null;
  completed_at: string | null;
}

export enum TaskStatus {
  PENDING = 'pending',
  IN_PROGRESS = 'in_progress',
  COMPLETED = 'completed',
  CANCELLED = 'cancelled',
  ON_HOLD = 'on_hold'
}

export enum TaskPriority {
  LOW = 'low',
  MEDIUM = 'medium',
  HIGH = 'high',
  CRITICAL = 'critical'
}

export interface CreateTaskRequest {
  title: string;
  description?: string;
  priority?: TaskPriority;
  tags?: string[];
  assigned_to?: string;
  due_date?: string;
}

export interface UpdateTaskRequest {
  title?: string;
  description?: string;
  status?: TaskStatus;
  priority?: TaskPriority;
  tags?: string[];
  assigned_to?: string;
  due_date?: string;
}

export interface ListTasksQuery {
  page_size?: number;
  page_token?: string;
  status?: TaskStatus;
  assigned_to?: string;
  tags?: string;
  sort_order?: SortOrder;
}

export enum SortOrder {
  CREATED_AT_ASC = 'created_at_asc',
  CREATED_AT_DESC = 'created_at_desc',
  DUE_DATE_ASC = 'due_date_asc',
  DUE_DATE_DESC = 'due_date_desc',
  PRIORITY_ASC = 'priority_asc',
  PRIORITY_DESC = 'priority_desc'
}

export interface ListTasksResponse {
  tasks: Task[];
  next_page_token: string | null;
  total_count: number;
}

export interface ApiError {
  code: string;
  message: string;
  details?: unknown;
}
```

### Server Implementation

```typescript
// code/typescript/rest/server/main.ts
import { Application, Router, Context } from "oak";
import { oakCors } from "cors";
import { TaskService } from "./services/task.service.ts";
import { taskRouter } from "./routes/task.routes.ts";
import { errorMiddleware } from "./middleware/error.middleware.ts";
import { loggingMiddleware } from "./middleware/logging.middleware.ts";
import { validationMiddleware } from "./middleware/validation.middleware.ts";

const app = new Application();
const PORT = parseInt(Deno.env.get("PORT") || "8080");

// Initialize services
const taskService = new TaskService();

// Global middleware
app.use(errorMiddleware);
app.use(loggingMiddleware);
app.use(oakCors()); // Enable CORS

// Health check
const healthRouter = new Router();
healthRouter.get("/health", (ctx: Context) => {
  ctx.response.body = {
    status: "healthy",
    timestamp: new Date().toISOString(),
    service: "task-api-typescript",
    version: "1.0.0",
    runtime: "Deno " + Deno.version.deno
  };
});

// API routes
const apiRouter = taskRouter(taskService);

// Mount routers
app.use(healthRouter.routes());
app.use(healthRouter.allowedMethods());
app.use(apiRouter.prefix("/api/v1").routes());
app.use(apiRouter.allowedMethods());

// Start server
console.log(`REST API server starting on http://localhost:${PORT}`);
console.log(`Health check: http://localhost:${PORT}/health`);
console.log(`API endpoint: http://localhost:${PORT}/api/v1/tasks`);

await app.listen({ port: PORT });
```

### Service Layer

```typescript
// code/typescript/rest/server/services/task.service.ts
import { v4 as uuid } from "uuid";
import {
  Task,
  TaskStatus,
  TaskPriority,
  CreateTaskRequest,
  UpdateTaskRequest,
  ListTasksQuery,
  ListTasksResponse,
  SortOrder
} from "../types/task.types.ts";

export class TaskService {
  private tasks: Map<string, Task> = new Map();

  constructor() {
    this.initializeSampleData();
  }

  private initializeSampleData(): void {
    const sampleTasks: CreateTaskRequest[] = [
      {
        title: "Implement TypeScript REST API",
        description: "Create REST API with Deno and Oak",
        priority: TaskPriority.HIGH,
        tags: ["typescript", "deno", "api"],
        assigned_to: "developer"
      },
      {
        title: "Add gRPC support",
        description: "Implement gRPC server with TypeScript",
        priority: TaskPriority.MEDIUM,
        tags: ["typescript", "grpc", "protobuf"],
        assigned_to: "developer"
      }
    ];

    sampleTasks.forEach(task => this.createTask(task));
  }

  async listTasks(query: ListTasksQuery): Promise<ListTasksResponse> {
    let tasks = Array.from(this.tasks.values());

    // Apply filters
    if (query.status) {
      tasks = tasks.filter(task => task.status === query.status);
    }

    if (query.assigned_to) {
      tasks = tasks.filter(task => task.assigned_to === query.assigned_to);
    }

    if (query.tags) {
      const filterTags = query.tags.split(',').map(t => t.trim());
      tasks = tasks.filter(task =>
        filterTags.every(tag => task.tags.includes(tag))
      );
    }

    // Apply sorting
    tasks = this.sortTasks(tasks, query.sort_order);

    // Apply pagination
    const pageSize = Math.min(query.page_size || 20, 100);
    const startIndex = query.page_token ? parseInt(query.page_token) : 0;
    const paginatedTasks = tasks.slice(startIndex, startIndex + pageSize);

    return {
      tasks: paginatedTasks,
      next_page_token: startIndex + pageSize < tasks.length
        ? String(startIndex + pageSize)
        : null,
      total_count: tasks.length
    };
  }

  async getTask(id: string): Promise<Task> {
    const task = this.tasks.get(id);
    if (!task) {
      throw new NotFoundError(`Task with ID ${id} not found`);
    }
    return task;
  }

  async createTask(request: CreateTaskRequest): Promise<Task> {
    const now = new Date().toISOString();
    const task: Task = {
      id: uuid.generate(),
      title: request.title,
      description: request.description || "",
      status: TaskStatus.PENDING,
      priority: request.priority || TaskPriority.MEDIUM,
      tags: request.tags || [],
      created_by: "system",
      assigned_to: request.assigned_to || null,
      created_at: now,
      updated_at: now,
      due_date: request.due_date || null,
      completed_at: null
    };

    this.tasks.set(task.id, task);
    return task;
  }

  async updateTask(id: string, request: UpdateTaskRequest): Promise<Task> {
    const task = await this.getTask(id);

    const updatedTask: Task = {
      ...task,
      ...request,
      id: task.id, // Prevent ID changes
      created_at: task.created_at, // Preserve creation time
      created_by: task.created_by, // Preserve creator
      updated_at: new Date().toISOString()
    };

    // Handle status change to completed
    if (request.status === TaskStatus.COMPLETED && task.status !== TaskStatus.COMPLETED) {
      updatedTask.completed_at = new Date().toISOString();
    }

    this.tasks.set(id, updatedTask);
    return updatedTask;
  }

  async deleteTask(id: string): Promise<void> {
    const task = await this.getTask(id);
    this.tasks.delete(id);
  }

  async updateTaskStatus(id: string, status: TaskStatus): Promise<Task> {
    return this.updateTask(id, { status });
  }

  private sortTasks(tasks: Task[], sortOrder?: SortOrder): Task[] {
    if (!sortOrder) return tasks;

    const sortFunctions: Record<SortOrder, (a: Task, b: Task) => number> = {
      [SortOrder.CREATED_AT_ASC]: (a, b) => 
        new Date(a.created_at).getTime() - new Date(b.created_at).getTime(),
      [SortOrder.CREATED_AT_DESC]: (a, b) => 
        new Date(b.created_at).getTime() - new Date(a.created_at).getTime(),
      [SortOrder.DUE_DATE_ASC]: (a, b) => {
        if (!a.due_date) return 1;
        if (!b.due_date) return -1;
        return new Date(a.due_date).getTime() - new Date(b.due_date).getTime();
      },
      [SortOrder.DUE_DATE_DESC]: (a, b) => {
        if (!a.due_date) return 1;
        if (!b.due_date) return -1;
        return new Date(b.due_date).getTime() - new Date(a.due_date).getTime();
      },
      [SortOrder.PRIORITY_ASC]: (a, b) => 
        this.getPriorityValue(a.priority) - this.getPriorityValue(b.priority),
      [SortOrder.PRIORITY_DESC]: (a, b) => 
        this.getPriorityValue(b.priority) - this.getPriorityValue(a.priority)
    };

    return tasks.sort(sortFunctions[sortOrder]);
  }

  private getPriorityValue(priority: TaskPriority): number {
    const values: Record<TaskPriority, number> = {
      [TaskPriority.LOW]: 1,
      [TaskPriority.MEDIUM]: 2,
      [TaskPriority.HIGH]: 3,
      [TaskPriority.CRITICAL]: 4
    };
    return values[priority];
  }
}

export class NotFoundError extends Error {
  constructor(message: string) {
    super(message);
    this.name = "NotFoundError";
  }
}

export class ValidationError extends Error {
  constructor(message: string) {
    super(message);
    this.name = "ValidationError";
  }
}
```

### Route Handlers

```typescript
// code/typescript/rest/server/routes/task.routes.ts
import { Router, Context } from "oak";
import { TaskService } from "../services/task.service.ts";
import { 
  CreateTaskRequest, 
  UpdateTaskRequest,
  ListTasksQuery,
  TaskStatus
} from "../types/task.types.ts";
import { validateCreateTask, validateUpdateTask } from "../middleware/validation.middleware.ts";

export function taskRouter(taskService: TaskService): Router {
  const router = new Router();

  // List tasks
  router.get("/tasks", async (ctx: Context) => {
    const query: ListTasksQuery = {
      page_size: parseInt(ctx.request.url.searchParams.get("page_size") || "20"),
      page_token: ctx.request.url.searchParams.get("page_token") || undefined,
      status: ctx.request.url.searchParams.get("status") as TaskStatus || undefined,
      assigned_to: ctx.request.url.searchParams.get("assigned_to") || undefined,
      tags: ctx.request.url.searchParams.get("tags") || undefined,
      sort_order: ctx.request.url.searchParams.get("sort_order") as any || undefined
    };

    const response = await taskService.listTasks(query);
    ctx.response.body = response;
  });

  // Get single task
  router.get("/tasks/:id", async (ctx: Context) => {
    const { id } = ctx.params;
    const task = await taskService.getTask(id!);
    ctx.response.body = task;
  });

  // Create task
  router.post("/tasks", validateCreateTask, async (ctx: Context) => {
    const body = await ctx.request.body().value as CreateTaskRequest;
    const task = await taskService.createTask(body);
    
    ctx.response.status = 201;
    ctx.response.headers.set("Location", `/api/v1/tasks/${task.id}`);
    ctx.response.body = task;
  });

  // Update task
  router.put("/tasks/:id", validateUpdateTask, async (ctx: Context) => {
    const { id } = ctx.params;
    const body = await ctx.request.body().value as UpdateTaskRequest;
    const task = await taskService.updateTask(id!, body);
    ctx.response.body = task;
  });

  // Update task status
  router.patch("/tasks/:id/status", async (ctx: Context) => {
    const { id } = ctx.params;
    const body = await ctx.request.body().value as { status: TaskStatus };
    
    if (!body.status) {
      ctx.response.status = 400;
      ctx.response.body = { error: "Status is required" };
      return;
    }

    const task = await taskService.updateTaskStatus(id!, body.status);
    ctx.response.body = task;
  });

  // Delete task
  router.delete("/tasks/:id", async (ctx: Context) => {
    const { id } = ctx.params;
    await taskService.deleteTask(id!);
    ctx.response.status = 204;
  });

  return router;
}
```

### Middleware

```typescript
// code/typescript/rest/server/middleware/error.middleware.ts
import { Context, Next } from "oak";
import { NotFoundError, ValidationError } from "../services/task.service.ts";

export async function errorMiddleware(ctx: Context, next: Next) {
  try {
    await next();
  } catch (error) {
    if (error instanceof NotFoundError) {
      ctx.response.status = 404;
      ctx.response.body = {
        error: {
          code: "NOT_FOUND",
          message: error.message
        }
      };
    } else if (error instanceof ValidationError) {
      ctx.response.status = 400;
      ctx.response.body = {
        error: {
          code: "VALIDATION_ERROR",
          message: error.message
        }
      };
    } else {
      console.error("Unhandled error:", error);
      ctx.response.status = 500;
      ctx.response.body = {
        error: {
          code: "INTERNAL_ERROR",
          message: "An internal error occurred"
        }
      };
    }
  }
}
```

```typescript
// code/typescript/rest/server/middleware/logging.middleware.ts
import { Context, Next } from "oak";

export async function loggingMiddleware(ctx: Context, next: Next) {
  const start = Date.now();
  
  await next();
  
  const ms = Date.now() - start;
  const rt = ctx.response.headers.get("X-Response-Time");
  
  console.log(
    `${ctx.request.method} ${ctx.request.url.pathname} - ${ctx.response.status} ${rt}`
  );
  
  ctx.response.headers.set("X-Response-Time", `${ms}ms`);
}
```

```typescript
// code/typescript/rest/server/middleware/validation.middleware.ts
import { Context, Next } from "oak";
import { TaskPriority, TaskStatus } from "../types/task.types.ts";
import { ValidationError } from "../services/task.service.ts";

export async function validateCreateTask(ctx: Context, next: Next) {
  const body = await ctx.request.body().value;
  
  const errors: string[] = [];
  
  if (!body.title || body.title.trim().length === 0) {
    errors.push("Title is required");
  }
  
  if (body.title && body.title.length > 200) {
    errors.push("Title must be 200 characters or less");
  }
  
  if (body.priority && !Object.values(TaskPriority).includes(body.priority)) {
    errors.push(`Priority must be one of: ${Object.values(TaskPriority).join(", ")}`);
  }
  
  if (errors.length > 0) {
    throw new ValidationError(errors.join(", "));
  }
  
  await next();
}

export async function validateUpdateTask(ctx: Context, next: Next) {
  const body = await ctx.request.body().value;
  
  const errors: string[] = [];
  
  if (body.title !== undefined) {
    if (body.title.trim().length === 0) {
      errors.push("Title cannot be empty");
    }
    if (body.title.length > 200) {
      errors.push("Title must be 200 characters or less");
    }
  }
  
  if (body.status && !Object.values(TaskStatus).includes(body.status)) {
    errors.push(`Status must be one of: ${Object.values(TaskStatus).join(", ")}`);
  }
  
  if (body.priority && !Object.values(TaskPriority).includes(body.priority)) {
    errors.push(`Priority must be one of: ${Object.values(TaskPriority).join(", ")}`);
  }
  
  if (errors.length > 0) {
    throw new ValidationError(errors.join(", "));
  }
  
  await next();
}
```

### REST Client Implementation

```typescript
// code/typescript/rest/client/client.ts
import { Task, CreateTaskRequest, UpdateTaskRequest, ListTasksQuery, ListTasksResponse, TaskStatus } from "../server/types/task.types.ts";

export class TaskAPIClient {
  private baseURL: string;
  private headers: HeadersInit;

  constructor(baseURL = "http://localhost:8080/api/v1") {
    this.baseURL = baseURL;
    this.headers = {
      "Content-Type": "application/json",
      "Accept": "application/json"
    };
  }

  setAuthToken(token: string): void {
    this.headers = {
      ...this.headers,
      "Authorization": `Bearer ${token}`
    };
  }

  setApiKey(apiKey: string): void {
    this.headers = {
      ...this.headers,
      "X-API-Key": apiKey
    };
  }

  private async request<T>(
    method: string,
    path: string,
    body?: unknown
  ): Promise<T> {
    const url = `${this.baseURL}${path}`;
    const options: RequestInit = {
      method,
      headers: this.headers
    };

    if (body && ["POST", "PUT", "PATCH"].includes(method)) {
      options.body = JSON.stringify(body);
    }

    const response = await fetch(url, options);

    if (!response.ok) {
      const error = await response.json();
      throw new Error(error.message || `HTTP ${response.status}`);
    }

    if (response.status === 204) {
      return null as T;
    }

    return await response.json();
  }

  async listTasks(query?: ListTasksQuery): Promise<ListTasksResponse> {
    const params = new URLSearchParams();
    if (query) {
      Object.entries(query).forEach(([key, value]) => {
        if (value !== undefined) {
          params.append(key, String(value));
        }
      });
    }
    return this.request<ListTasksResponse>("GET", `/tasks?${params}`);
  }

  async getTask(id: string): Promise<Task> {
    return this.request<Task>("GET", `/tasks/${id}`);
  }

  async createTask(task: CreateTaskRequest): Promise<Task> {
    return this.request<Task>("POST", "/tasks", task);
  }

  async updateTask(id: string, updates: UpdateTaskRequest): Promise<Task> {
    return this.request<Task>("PUT", `/tasks/${id}`, updates);
  }

  async updateTaskStatus(id: string, status: TaskStatus): Promise<Task> {
    return this.request<Task>("PATCH", `/tasks/${id}/status`, { status });
  }

  async deleteTask(id: string): Promise<void> {
    return this.request<void>("DELETE", `/tasks/${id}`);
  }
}

// Example usage
async function demonstrateClient() {
  const client = new TaskAPIClient();

  try {
    // Create a task
    const newTask = await client.createTask({
      title: "Learn TypeScript with Deno",
      description: "Complete Chapter 2 of the book",
      priority: TaskPriority.HIGH,
      tags: ["learning", "typescript", "deno"]
    });
    console.log("Created task:", newTask);

    // List all tasks
    const response = await client.listTasks({
      status: TaskStatus.PENDING,
      sort_order: SortOrder.PRIORITY_DESC
    });
    console.log(`Found ${response.tasks.length} pending tasks`);

    // Update task status
    const updated = await client.updateTaskStatus(newTask.id, TaskStatus.IN_PROGRESS);
    console.log("Updated task status:", updated.status);

    // Delete task
    await client.deleteTask(newTask.id);
    console.log("Task deleted successfully");

  } catch (error) {
    console.error("Client demo failed:", error);
  }
}

// Run if called directly
if (import.meta.main) {
  await demonstrateClient();
}
```

## gRPC Implementation

Now let's implement the gRPC service using TypeScript. We'll use the `@grpc/grpc-js` package with TypeScript type generation.

### Protocol Buffer Type Generation

```typescript
// code/typescript/grpc/scripts/generate-types.ts
// This script generates TypeScript types from proto files
import { exec } from "https://deno.land/std@0.203.0/process/mod.ts";

const PROTO_PATH = "../../../shared/protos/tasks.proto";
const OUTPUT_PATH = "./generated";

async function generateTypes() {
  // Install necessary tools
  const installCmd = [
    "npm", "install", "-g",
    "grpc-tools",
    "grpc_tools_node_protoc_ts"
  ];

  // Generate JavaScript code
  const jsCmd = [
    "grpc_tools_node_protoc",
    `--js_out=import_style=commonjs,binary:${OUTPUT_PATH}`,
    `--grpc_out=grpc_js:${OUTPUT_PATH}`,
    `--proto_path=../../../shared/protos`,
    "tasks.proto"
  ];

  // Generate TypeScript definitions
  const tsCmd = [
    "protoc",
    `--plugin=protoc-gen-ts=./node_modules/.bin/protoc-gen-ts`,
    `--ts_out=grpc_js:${OUTPUT_PATH}`,
    `--proto_path=../../../shared/protos`,
    "tasks.proto"
  ];

  console.log("Generating gRPC types...");
  
  try {
    await exec(installCmd);
    await exec(jsCmd);
    await exec(tsCmd);
    console.log("Types generated successfully!");
  } catch (error) {
    console.error("Failed to generate types:", error);
  }
}

if (import.meta.main) {
  await generateTypes();
}
```

### gRPC Server Implementation

```typescript
// code/typescript/grpc/server/server.ts
import * as grpc from "@grpc/grpc-js";
import * as protoLoader from "@grpc/proto-loader";
import { TaskServiceImpl } from "./services/task.service.ts";
import { fileURLToPath } from "url";
import { dirname, join } from "path";

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

// Load proto file
const PROTO_PATH = join(__dirname, "../../../shared/protos/tasks.proto");

const packageDefinition = protoLoader.loadSync(PROTO_PATH, {
  keepCase: true,
  longs: String,
  enums: String,
  defaults: true,
  oneofs: true
});

const tasksProto = grpc.loadPackageDefinition(packageDefinition) as any;

// Create server
const server = new grpc.Server();

// Add service implementation
const taskService = new TaskServiceImpl();

server.addService(tasksProto.tasks.v1.TaskService.service, {
  ListTasks: taskService.listTasks.bind(taskService),
  GetTask: taskService.getTask.bind(taskService),
  CreateTask: taskService.createTask.bind(taskService),
  UpdateTask: taskService.updateTask.bind(taskService),
  DeleteTask: taskService.deleteTask.bind(taskService),
  WatchTasks: taskService.watchTasks.bind(taskService)
});

// Start server
const PORT = process.env.GRPC_PORT || "50051";
server.bindAsync(
  `0.0.0.0:${PORT}`,
  grpc.ServerCredentials.createInsecure(),
  (err: Error | null, port: number) => {
    if (err) {
      console.error("Failed to bind server:", err);
      return;
    }
    console.log(`gRPC server running on port ${port}`);
    console.log("Available services:");
    console.log("  - TaskService.ListTasks (server streaming)");
    console.log("  - TaskService.GetTask (unary)");
    console.log("  - TaskService.CreateTask (unary)");
    console.log("  - TaskService.UpdateTask (unary)");
    console.log("  - TaskService.DeleteTask (unary)");
    console.log("  - TaskService.WatchTasks (bidirectional streaming)");
  }
);

// Graceful shutdown
process.on("SIGTERM", () => {
  console.log("SIGTERM received, shutting down gracefully");
  server.tryShutdown(() => {
    console.log("Server shutdown complete");
  });
});
```

### gRPC Service Implementation

```typescript
// code/typescript/grpc/server/services/task.service.ts
import * as grpc from "@grpc/grpc-js";
import { v4 as uuidv4 } from "uuid";

interface Task {
  id: string;
  title: string;
  description: string;
  status: string;
  priority: string;
  tags: string[];
  created_by: string;
  assigned_to: string;
  created_at: Timestamp;
  updated_at: Timestamp;
  due_date?: Timestamp;
  completed_at?: Timestamp;
}

interface Timestamp {
  seconds: number;
  nanos: number;
}

export class TaskServiceImpl {
  private tasks: Map<string, Task> = new Map();
  private watchers: Set<grpc.ServerDuplexStream<any, any>> = new Set();

  constructor() {
    this.initializeSampleData();
  }

  private initializeSampleData(): void {
    const sampleTasks = [
      {
        id: uuidv4(),
        title: "Implement TypeScript gRPC server",
        description: "Create gRPC server with TypeScript",
        status: "TASK_STATUS_IN_PROGRESS",
        priority: "TASK_PRIORITY_HIGH",
        tags: ["grpc", "typescript", "server"],
        created_by: "system",
        assigned_to: "developer",
        created_at: this.getCurrentTimestamp(),
        updated_at: this.getCurrentTimestamp()
      }
    ];

    sampleTasks.forEach(task => {
      this.tasks.set(task.id, task);
    });
  }

  private getCurrentTimestamp(): Timestamp {
    const now = new Date();
    return {
      seconds: Math.floor(now.getTime() / 1000),
      nanos: (now.getTime() % 1000) * 1000000
    };
  }

  // Server streaming: List tasks
  listTasks(call: grpc.ServerWritableStream<any, Task>): void {
    const request = call.request;
    let tasks = Array.from(this.tasks.values());

    // Apply filters
    if (request.status && request.status !== "TASK_STATUS_UNSPECIFIED") {
      tasks = tasks.filter((task: Task) => task.status === request.status);
    }

    if (request.assigned_to) {
      tasks = tasks.filter((task: Task) => task.assigned_to === request.assigned_to);
    }

    if (request.tags && request.tags.length > 0) {
      tasks = tasks.filter((task: Task) =>
        request.tags.every((tag: string) => task.tags.includes(tag))
      );
    }

    // Stream tasks to client
    tasks.forEach((task: Task) => {
      call.write(task);
    });

    call.end();
  }

  // Unary call: Get single task
  getTask(
    call: grpc.ServerUnaryCall<any, Task>,
    callback: grpc.sendUnaryData<Task>
  ): void {
    const { id } = call.request;
    const task = this.tasks.get(id);

    if (!task) {
      return callback({
        code: grpc.status.NOT_FOUND,
        message: `Task with ID ${id} not found`
      });
    }

    callback(null, task);
  }

  // Unary call: Create task
  createTask(
    call: grpc.ServerUnaryCall<any, Task>,
    callback: grpc.sendUnaryData<Task>
  ): void {
    const { task } = call.request;

    if (!task.title) {
      return callback({
        code: grpc.status.INVALID_ARGUMENT,
        message: "Task title is required"
      });
    }

    const newTask: Task = {
      id: uuidv4(),
      title: task.title,
      description: task.description || "",
      status: task.status || "TASK_STATUS_PENDING",
      priority: task.priority || "TASK_PRIORITY_MEDIUM",
      tags: task.tags || [],
      created_by: task.created_by || "system",
      assigned_to: task.assigned_to || "",
      created_at: this.getCurrentTimestamp(),
      updated_at: this.getCurrentTimestamp(),
      due_date: task.due_date || undefined,
      completed_at: undefined
    };

    this.tasks.set(newTask.id, newTask);

    // Notify watchers
    this.notifyWatchers("EVENT_TYPE_CREATED", newTask);

    callback(null, newTask);
  }

  // Unary call: Update task
  updateTask(
    call: grpc.ServerUnaryCall<any, Task>,
    callback: grpc.sendUnaryData<Task>
  ): void {
    const { task, update_mask } = call.request;

    if (!task.id) {
      return callback({
        code: grpc.status.INVALID_ARGUMENT,
        message: "Task ID is required"
      });
    }

    const existingTask = this.tasks.get(task.id);
    if (!existingTask) {
      return callback({
        code: grpc.status.NOT_FOUND,
        message: `Task with ID ${task.id} not found`
      });
    }

    // Apply updates
    const updatedTask = { ...existingTask };

    if (update_mask && update_mask.length > 0) {
      update_mask.forEach((field: string) => {
        if (task[field] !== undefined) {
          (updatedTask as any)[field] = task[field];
        }
      });
    } else {
      Object.keys(task).forEach(key => {
        if (task[key] !== undefined && key !== "id" && key !== "created_at") {
          (updatedTask as any)[key] = task[key];
        }
      });
    }

    updatedTask.updated_at = this.getCurrentTimestamp();

    // Handle status change to completed
    if (
      updatedTask.status === "TASK_STATUS_COMPLETED" &&
      existingTask.status !== "TASK_STATUS_COMPLETED"
    ) {
      updatedTask.completed_at = this.getCurrentTimestamp();
    }

    this.tasks.set(task.id, updatedTask);

    // Notify watchers
    this.notifyWatchers("EVENT_TYPE_UPDATED", updatedTask);

    callback(null, updatedTask);
  }

  // Unary call: Delete task
  deleteTask(
    call: grpc.ServerUnaryCall<any, any>,
    callback: grpc.sendUnaryData<any>
  ): void {
    const { id } = call.request;
    const task = this.tasks.get(id);

    if (!task) {
      return callback({
        code: grpc.status.NOT_FOUND,
        message: `Task with ID ${id} not found`
      });
    }

    this.tasks.delete(id);

    // Notify watchers
    this.notifyWatchers("EVENT_TYPE_DELETED", task);

    callback(null, {});
  }

  // Bidirectional streaming: Watch tasks
  watchTasks(call: grpc.ServerDuplexStream<any, any>): void {
    // Add this call to watchers
    this.watchers.add(call);

    // Handle incoming requests
    call.on("data", (request: any) => {
      console.log("Watch request received:", request);

      // Send current tasks matching the filter
      if (request.watch_all) {
        this.tasks.forEach((task: Task) => {
          call.write({
            event_type: "EVENT_TYPE_CREATED",
            task: task,
            timestamp: this.getCurrentTimestamp()
          });
        });
      } else if (request.task_ids && request.task_ids.length > 0) {
        request.task_ids.forEach((id: string) => {
          const task = this.tasks.get(id);
          if (task) {
            call.write({
              event_type: "EVENT_TYPE_CREATED",
              task: task,
              timestamp: this.getCurrentTimestamp()
            });
          }
        });
      }
    });

    // Handle client disconnect
    call.on("end", () => {
      this.watchers.delete(call);
      call.end();
    });

    // Handle errors
    call.on("error", (err: Error) => {
      console.error("Watch stream error:", err);
      this.watchers.delete(call);
    });
  }

  // Helper: Notify all watchers of task changes
  private notifyWatchers(eventType: string, task: Task): void {
    const event = {
      event_type: eventType,
      task: task,
      timestamp: this.getCurrentTimestamp()
    };

    this.watchers.forEach(watcher => {
      try {
        watcher.write(event);
      } catch (err) {
        console.error("Failed to notify watcher:", err);
        this.watchers.delete(watcher);
      }
    });
  }
}
```

## Testing and Benchmarks

```typescript
// code/typescript/test/benchmark.ts
import { TaskAPIClient as RestClient } from "../rest/client/client.ts";
import { TaskGrpcClient } from "../grpc/client/client.ts";

interface BenchmarkResult {
  min: number;
  max: number;
  avg: number;
  median: number;
}

class PerformanceBenchmark {
  private restClient: RestClient;
  private grpcClient: TaskGrpcClient;

  constructor() {
    this.restClient = new RestClient();
    this.grpcClient = new TaskGrpcClient();
  }

  async measureLatency(
    fn: () => Promise<any>,
    iterations = 100
  ): Promise<BenchmarkResult> {
    const times: number[] = [];

    for (let i = 0; i < iterations; i++) {
      const start = performance.now();
      await fn();
      const end = performance.now();
      times.push(end - start);
    }

    times.sort((a, b) => a - b);

    return {
      min: Math.min(...times),
      max: Math.max(...times),
      avg: times.reduce((a, b) => a + b, 0) / times.length,
      median: times[Math.floor(times.length / 2)]
    };
  }

  async runBenchmarks(): Promise<void> {
    console.log("\n=== TypeScript Performance Benchmarks ===\n");

    // Test data
    const testTask = {
      title: "Benchmark Task",
      description: "Performance testing",
      priority: "medium" as const
    };

    // Benchmark CREATE operations
    console.log("CREATE Operation:");
    const restCreate = await this.measureLatency(
      () => this.restClient.createTask(testTask),
      50
    );
    const grpcCreate = await this.measureLatency(
      () => this.grpcClient.createTask({
        ...testTask,
        priority: "TASK_PRIORITY_MEDIUM"
      }),
      50
    );

    console.log(`  REST: avg ${restCreate.avg.toFixed(2)}ms, median ${restCreate.median.toFixed(2)}ms`);
    console.log(`  gRPC: avg ${grpcCreate.avg.toFixed(2)}ms, median ${grpcCreate.median.toFixed(2)}ms`);
    console.log(`  gRPC is ${(restCreate.avg / grpcCreate.avg).toFixed(2)}x faster\n`);

    // Benchmark LIST operations
    console.log("LIST Operation:");
    const restList = await this.measureLatency(
      () => this.restClient.listTasks(),
      100
    );
    const grpcList = await this.measureLatency(
      () => this.grpcClient.listTasks(),
      100
    );

    console.log(`  REST: avg ${restList.avg.toFixed(2)}ms, median ${restList.median.toFixed(2)}ms`);
    console.log(`  gRPC: avg ${grpcList.avg.toFixed(2)}ms, median ${grpcList.median.toFixed(2)}ms`);
    console.log(`  gRPC is ${(restList.avg / grpcList.avg).toFixed(2)}x faster\n`);
  }
}

// Run benchmarks
if (import.meta.main) {
  const benchmark = new PerformanceBenchmark();
  await benchmark.runBenchmarks();
  console.log("Benchmarks complete!");
}
```

## Best Practices and Patterns

### TypeScript-Specific Best Practices

1. **Strict Type Checking**: Always enable strict mode in `tsconfig.json`
2. **Avoid `any`**: Use `unknown` when type is truly unknown, then narrow it
3. **Const Assertions**: Use `as const` for literal types
4. **Discriminated Unions**: Leverage for type-safe state machines
5. **Utility Types**: Master `Partial`, `Required`, `Pick`, `Omit`, etc.

### Type Safety Patterns

```typescript
// Discriminated unions for request/response types
type ApiResponse<T> = 
  | { success: true; data: T }
  | { success: false; error: string };

// Builder pattern with method chaining
class TaskBuilder {
  private task: Partial<Task> = {};

  title(title: string): this {
    this.task.title = title;
    return this;
  }

  priority(priority: TaskPriority): this {
    this.task.priority = priority;
    return this;
  }

  build(): Task {
    if (!this.task.title) {
      throw new Error("Title is required");
    }
    return this.task as Task;
  }
}

// Type guards for runtime validation
function isTask(obj: unknown): obj is Task {
  return (
    typeof obj === "object" &&
    obj !== null &&
    "id" in obj &&
    "title" in obj &&
    "status" in obj
  );
}
```

### Error Handling

```typescript
// Custom error types with proper inheritance
class DomainError extends Error {
  constructor(
    message: string,
    public readonly code: string,
    public readonly statusCode: number
  ) {
    super(message);
    this.name = this.constructor.name;
    Error.captureStackTrace(this, this.constructor);
  }
}

class NotFoundError extends DomainError {
  constructor(resource: string, id: string) {
    super(
      `${resource} with ID ${id} not found`,
      "NOT_FOUND",
      404
    );
  }
}

// Result type for functional error handling
type Result<T, E = Error> = 
  | { ok: true; value: T }
  | { ok: false; error: E };

function tryCreateTask(data: unknown): Result<Task> {
  try {
    // Validation and creation logic
    return { ok: true, value: createdTask };
  } catch (error) {
    return { ok: false, error };
  }
}
```

## Quick Reference

### Common Commands

```bash
# Deno commands
deno run --allow-net --allow-env main.ts
deno test --allow-net
deno fmt
deno lint
deno compile --allow-net --allow-env -o task-api main.ts

# TypeScript compiler
tsc --init
tsc --watch
tsc --noEmit  # Type check only

# Type generation for gRPC
npm install -g grpc-tools grpc_tools_node_protoc_ts
grpc_tools_node_protoc --ts_out=. tasks.proto
```

### Type Utilities

```typescript
// Extract types from constants
const STATUSES = ["pending", "completed"] as const;
type Status = typeof STATUSES[number]; // "pending" | "completed"

// Mapped types
type Nullable<T> = { [K in keyof T]: T[K] | null };
type DeepPartial<T> = { [K in keyof T]?: DeepPartial<T[K]> };

// Template literal types
type HTTPMethod = "GET" | "POST" | "PUT" | "DELETE";
type Endpoint = `/api/v1/${string}`;
type Route = `${HTTPMethod} ${Endpoint}`;
```

## Conclusion

TypeScript brings the best of both worlds: JavaScript's flexibility and ecosystem with the safety and tooling of static typing. The implementations in this chapter demonstrate how TypeScript's type system can catch errors at compile time, provide excellent IDE support, and make refactoring safer.

Key takeaways from the TypeScript implementation:

1. **Type Safety Without Runtime Overhead**: Types are erased at compile time
2. **Gradual Adoption**: You can add types incrementally to existing JavaScript
3. **Powerful Type System**: Advanced features like conditional and mapped types
4. **Ecosystem Compatibility**: Use any JavaScript library with or without types
5. **Developer Experience**: Unparalleled IDE support and refactoring capabilities

TypeScript has proven that dynamic languages can adopt static typing successfully. Whether you're building a small API or a large enterprise application, TypeScript provides the tools to write more maintainable, less error-prone code while keeping the development experience pleasant and productive.