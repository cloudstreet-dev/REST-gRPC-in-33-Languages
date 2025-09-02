# Haskell REST API Implementation

This directory contains a REST API implementation in Haskell using the Servant framework, demonstrating pure functional programming principles.

## Features

- **Type-safe REST API** with Servant's type-level DSL
- **Pure functional design** with immutable data structures
- **STM (Software Transactional Memory)** for thread-safe state management
- **Comprehensive type system** ensuring correctness at compile time
- **Lazy evaluation** for efficient resource usage
- **Monadic composition** for clean error handling

## Prerequisites

- GHC (Glasgow Haskell Compiler) 9.0 or higher
- Cabal or Stack build tool

## Server

The server implements a complete REST API using Servant, a type-safe web framework.

### Building and Running

Using Cabal:
```bash
cd server
cabal build
cabal run task-server
```

Using Stack:
```bash
cd server
stack build
stack exec task-server
```

The server will start on port 8080 by default.

### API Endpoints

- `GET /api/tasks` - List all tasks (with filtering and pagination)
- `GET /api/tasks/:id` - Get a specific task
- `POST /api/tasks` - Create a new task
- `PUT /api/tasks/:id` - Update a task
- `PATCH /api/tasks/:id/status` - Update task status
- `DELETE /api/tasks/:id` - Delete a task
- `GET /health` - Health check

### Query Parameters

- `status` - Filter by task status (pending, in_progress, completed, cancelled)
- `assigned_to` - Filter by assignee
- `tags` - Filter by tags (comma-separated)
- `page_size` - Number of results per page (default: 20, max: 100)
- `page_token` - Pagination token
- `sort_by` - Sort field (created_at, updated_at, title)
- `sort_order` - Sort order (asc, desc)

## Client

The client provides a type-safe SDK for interacting with the REST API.

### Building and Running

```bash
cd client
cabal build
cabal run task-client
```

### Using the Client Library

```haskell
import Client
import Network.HTTP.Client.TLS

main :: IO ()
main = do
    manager <- newManager tlsManagerSettings
    
    -- List all tasks
    Right response <- listTasks manager
    print $ tasks response
    
    -- Create a task
    let newTask = CreateTaskRequest
            { createTitle = "Learn Haskell"
            , createDescription = Just "Master functional programming"
            , createPriority = Just "high"
            , createTags = Just ["haskell", "learning"]
            , createAssignedTo = Just "me"
            }
    Right task <- createTask manager newTask
    
    -- Update task status
    Right updated <- updateTaskStatus manager (taskId task) "in_progress"
    
    -- Delete task
    Right () <- deleteTask manager (taskId task)
```

## Architecture

### Type-Safe API Definition

Servant uses type-level programming to define APIs:

```haskell
type TaskAPI = "api" :> "tasks" :> Get '[JSON] [Task]
          :<|> "api" :> "tasks" :> Capture "id" UUID :> Get '[JSON] Task
          :<|> "api" :> "tasks" :> ReqBody '[JSON] CreateTaskRequest 
                                :> PostCreated '[JSON] Task
```

### STM for Concurrency

Software Transactional Memory ensures thread-safe state updates:

```haskell
updateTask :: Repository -> UUID -> UpdateRequest -> IO (Maybe Task)
updateTask repo taskId updates = atomically $ do
    taskMap <- readTVar repo
    case Map.lookup taskId taskMap of
        Nothing -> return Nothing
        Just task -> do
            let updated = applyUpdates task updates
            writeTVar repo $ Map.insert taskId updated taskMap
            return $ Just updated
```

### Monadic Error Handling

```haskell
getTaskHandler :: UUID -> Handler Task
getTaskHandler taskId = do
    maybeTask <- liftIO $ getTask repo taskId
    case maybeTask of
        Nothing -> throwError err404 { errBody = "Task not found" }
        Just task -> return task
```

## Haskell Features Demonstrated

### Algebraic Data Types

```haskell
data TaskStatus = Pending 
                | InProgress 
                | Completed 
                | Cancelled
    deriving (Show, Eq, Generic)

data Task = Task
    { taskId :: UUID
    , taskTitle :: Text
    , taskStatus :: TaskStatus
    , taskPriority :: TaskPriority
    } deriving (Show, Eq, Generic)
```

### Type Classes

```haskell
class ToJSON a where
    toJSON :: a -> Value

instance ToJSON Task where
    toJSON task = object
        [ "id" .= taskId task
        , "title" .= taskTitle task
        , "status" .= taskStatus task
        ]
```

### Functors and Applicatives

```haskell
-- Functor
fmap (+1) (Just 5)  -- Just 6

-- Applicative
CreateTaskRequest 
    <$> v .: "title"
    <*> v .:? "description"
    <*> v .:? "priority"
```

### Monads

```haskell
do
    taskId <- nextRandom
    now <- getCurrentTime
    let task = Task taskId title Pending Medium [] now
    saveTask task
    return task
```

## Testing

### Unit Tests with HUnit

```haskell
import Test.HUnit

testCreateTask :: Test
testCreateTask = TestCase $ do
    repo <- newRepository
    task <- createTaskInRepo repo $ CreateTaskRequest "Test" Nothing Nothing
    assertEqual "Task title" "Test" (taskTitle task)
    assertEqual "Task status" Pending (taskStatus task)
```

### Property Testing with QuickCheck

```haskell
import Test.QuickCheck

prop_taskUpdate :: Task -> UpdateRequest -> Bool
prop_taskUpdate task updates =
    let updated = applyUpdates task updates
    in taskId updated == taskId task &&
       taskUpdatedAt updated >= taskCreatedAt task
```

## Performance Optimization

### Lazy Evaluation

```haskell
-- Only computed when needed
infiniteTasks :: [Task]
infiniteTasks = [createTask n | n <- [1..]]

-- Take only what's needed
firstTenTasks = take 10 infiniteTasks
```

### Strictness Annotations

```haskell
data Task = Task
    { taskId :: !UUID          -- Strict field
    , taskTitle :: !Text       -- Evaluated immediately
    , taskDescription :: Text  -- Lazy field
    }
```

## Docker Support

```dockerfile
FROM haskell:9.4

WORKDIR /app

# Copy cabal file first for dependency caching
COPY task-server.cabal ./
RUN cabal update && cabal build --only-dependencies

# Copy source and build
COPY . .
RUN cabal build

EXPOSE 8080
CMD ["cabal", "run", "task-server"]
```

## Best Practices

1. **Use type signatures**: Always provide explicit type signatures
2. **Leverage the type system**: Make illegal states unrepresentable
3. **Prefer pure functions**: Minimize IO and side effects
4. **Use appropriate abstractions**: Functors, Applicatives, Monads
5. **Handle errors with types**: Maybe, Either instead of exceptions
6. **Test with properties**: QuickCheck for comprehensive testing

## Common Patterns

### Smart Constructors

```haskell
mkTask :: Text -> Maybe Task
mkTask title
    | Text.null title = Nothing
    | otherwise = Just $ Task { ... }
```

### Phantom Types

```haskell
newtype Validated a = Validated a
newtype Unvalidated a = Unvalidated a

validateTask :: Task Unvalidated -> Maybe (Task Validated)
```

## Dependencies

### Server
- `servant` - Type-safe REST API framework
- `servant-server` - Server implementation
- `warp` - High-performance web server
- `aeson` - JSON serialization
- `stm` - Software Transactional Memory
- `uuid` - UUID generation

### Client
- `http-client` - HTTP client library
- `http-client-tls` - TLS support
- `aeson` - JSON parsing