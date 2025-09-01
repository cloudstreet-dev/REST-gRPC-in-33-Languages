import Foundation
import GRPC
import NIOCore
import NIOPosix
import ArgumentParser
import SwiftProtobuf
// import Generated  // This will contain the generated proto files

class TaskGrpcClient {
    private let channel: GRPCChannel
    // private let client: Task_TaskServiceAsyncClient
    
    init(host: String, port: Int) throws {
        let group = PlatformSupport.makeEventLoopGroup(loopCount: 1)
        
        self.channel = try GRPCChannelPool.with(
            target: .host(host, port: port),
            transportSecurity: .plaintext,
            eventLoopGroup: group
        )
        
        // self.client = Task_TaskServiceAsyncClient(channel: channel)
    }
    
    deinit {
        try? channel.close().wait()
    }
    
    /* 
    // Note: This implementation requires proto file generation
    // The following methods will work after running protoc with Swift gRPC plugin
    
    func listTasks(pageSize: Int32 = 20,
                   pageToken: String = "",
                   status: Task_TaskStatus = .unspecified,
                   assignedTo: String = "",
                   tags: [String] = [],
                   sortOrder: Task_SortOrder = .unspecified) async throws -> [Task_Task] {
        
        let request = Task_ListTasksRequest.with {
            $0.pageSize = pageSize
            $0.pageToken = pageToken
            $0.status = status
            $0.assignedTo = assignedTo
            $0.tags = tags
            $0.sortOrder = sortOrder
        }
        
        var tasks: [Task_Task] = []
        let responseStream = client.listTasks(request)
        
        for try await task in responseStream {
            tasks.append(task)
        }
        
        return tasks
    }
    
    func getTask(id: String) async throws -> Task_Task? {
        let request = Task_GetTaskRequest.with {
            $0.id = id
        }
        
        do {
            return try await client.getTask(request)
        } catch let error as GRPCStatus where error.code == .notFound {
            return nil
        }
    }
    
    func createTask(title: String,
                    description: String = "",
                    priority: Task_TaskPriority = .medium,
                    tags: [String] = [],
                    assignedTo: String = "") async throws -> Task_Task {
        
        let task = Task_Task.with {
            $0.title = title
            $0.description_p = description
            $0.priority = priority
            $0.tags = tags
            $0.assignedTo = assignedTo
        }
        
        let request = Task_CreateTaskRequest.with {
            $0.task = task
        }
        
        return try await client.createTask(request)
    }
    
    func updateTask(id: String,
                    title: String? = nil,
                    description: String? = nil,
                    status: Task_TaskStatus? = nil,
                    priority: Task_TaskPriority? = nil,
                    tags: [String]? = nil,
                    assignedTo: String? = nil) async throws -> Task_Task? {
        
        var taskBuilder = Task_Task()
        taskBuilder.id = id
        var updateMask: [String] = []
        
        if let title = title {
            taskBuilder.title = title
            updateMask.append("title")
        }
        
        if let description = description {
            taskBuilder.description_p = description
            updateMask.append("description")
        }
        
        if let status = status {
            taskBuilder.status = status
            updateMask.append("status")
        }
        
        if let priority = priority {
            taskBuilder.priority = priority
            updateMask.append("priority")
        }
        
        if let tags = tags {
            taskBuilder.tags = tags
            updateMask.append("tags")
        }
        
        if let assignedTo = assignedTo {
            taskBuilder.assignedTo = assignedTo
            updateMask.append("assigned_to")
        }
        
        let request = Task_UpdateTaskRequest.with {
            $0.task = taskBuilder
            $0.updateMask = updateMask
        }
        
        do {
            return try await client.updateTask(request)
        } catch let error as GRPCStatus where error.code == .notFound {
            return nil
        }
    }
    
    func deleteTask(id: String) async throws -> Bool {
        let request = Task_DeleteTaskRequest.with {
            $0.id = id
        }
        
        do {
            _ = try await client.deleteTask(request)
            return true
        } catch let error as GRPCStatus where error.code == .notFound {
            return false
        }
    }
    
    func watchTasks(watchAll: Bool = false,
                    taskIds: [String] = [],
                    assignedTo: String = "") -> AsyncThrowingStream<Task_TaskEvent, Error> {
        
        return AsyncThrowingStream { continuation in
            Task {
                do {
                    let requestStream = AsyncStream<Task_WatchTasksRequest> { streamContinuation in
                        let request = Task_WatchTasksRequest.with {
                            $0.watchAll = watchAll
                            $0.taskIds = taskIds
                            $0.assignedTo = assignedTo
                        }
                        streamContinuation.yield(request)
                        streamContinuation.finish()
                    }
                    
                    let responseStream = client.watchTasks(requestStream: requestStream.map { $0 })
                    
                    for try await event in responseStream {
                        continuation.yield(event)
                    }
                    
                    continuation.finish()
                } catch {
                    continuation.finish(throwing: error)
                }
            }
        }
    }
    */
}

@main
struct TaskGrpcClientApp: AsyncParsableCommand {
    static let configuration = CommandConfiguration(
        commandName: "task-grpc-client",
        abstract: "A Swift gRPC client for the Task Management API"
    )
    
    @Option(name: .long, help: "Server host")
    var host: String = "localhost"
    
    @Option(name: .long, help: "Server port")
    var port: Int = 50051
    
    func run() async throws {
        print("Swift Task gRPC Client")
        print("====================")
        print("")
        print("Note: This is a template implementation.")
        print("To use this client, you need to:")
        print("1. Generate Swift protobuf files from tasks.proto")
        print("2. Add the generated files to the Generated target")
        print("3. Uncomment the client implementation above")
        print("")
        print("Run the following command to generate Swift files:")
        print("protoc --swift_out=Sources/Generated --grpc-swift_out=Sources/Generated ../../../../shared/protos/tasks.proto")
        print("")
        
        /* 
        // Uncomment after proto generation:
        
        let client = try TaskGrpcClient(host: host, port: port)
        
        do {
            // Create a task
            print("Creating a new task...")
            let task = try await client.createTask(
                title: "Test Swift gRPC Client",
                description: "Testing the Swift gRPC client implementation",
                priority: .high,
                tags: ["test", "swift", "grpc"],
                assignedTo: "dev-team"
            )
            print("Created task: \(task.id) - \(task.title)")
            
            // Get the task
            print("\nRetrieving task \(task.id)...")
            if let retrieved = try await client.getTask(id: task.id) {
                print("Retrieved task: \(retrieved.title) - Status: \(retrieved.status)")
            }
            
            // Update task status
            print("\nUpdating task status to IN_PROGRESS...")
            if let updated = try await client.updateTask(
                id: task.id,
                status: .inProgress
            ) {
                print("Updated task status: \(updated.status)")
            }
            
            // List all tasks
            print("\nListing all tasks...")
            let tasks = try await client.listTasks(
                pageSize: 10,
                sortOrder: .createdDesc
            )
            for t in tasks {
                print("[\(t.status)] \(t.title) - \(t.id)")
            }
            
            // Watch for task changes
            print("\nWatching for task changes...")
            var eventCount = 0
            let watchStream = client.watchTasks(watchAll: true)
            
            for try await event in watchStream {
                print("Event: \(event.eventType) - Task: \(event.task.title)")
                eventCount += 1
                if eventCount >= 3 {
                    break
                }
            }
            
            // Delete the task
            print("\nDeleting task \(task.id)...")
            let deleted = try await client.deleteTask(id: task.id)
            print("Task deleted: \(deleted)")
            
        } catch {
            print("Error: \(error)")
        }
        */
    }
}