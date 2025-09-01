import Foundation
import GRPC
import NIOCore
import NIOPosix
import ArgumentParser
import SwiftProtobuf
// import Generated  // This will contain the generated proto files

class TaskServiceProvider {
    private var tasks: [String: Task_Task] = [:]
    private let queue = DispatchQueue(label: "task-service", attributes: .concurrent)
    
    init() {
        initializeSampleData()
    }
    
    private func initializeSampleData() {
        let now = Google_Protobuf_Timestamp.with {
            $0.seconds = Int64(Date().timeIntervalSince1970)
            $0.nanos = 0
        }
        
        let task1 = Task_Task.with {
            $0.id = UUID().uuidString
            $0.title = "Complete project documentation"
            $0.description_p = "Write comprehensive documentation for the gRPC API"
            $0.status = .inProgress
            $0.priority = .high
            $0.tags = ["documentation", "api"]
            $0.assignedTo = "dev-team"
            $0.createdAt = now
            $0.updatedAt = now
        }
        
        let task2 = Task_Task.with {
            $0.id = UUID().uuidString
            $0.title = "Review pull requests"
            $0.description_p = "Review and approve pending pull requests"
            $0.status = .pending
            $0.priority = .medium
            $0.tags = ["review", "code"]
            $0.assignedTo = "senior-dev"
            $0.createdAt = now
            $0.updatedAt = now
        }
        
        let task3 = Task_Task.with {
            $0.id = UUID().uuidString
            $0.title = "Deploy to production"
            $0.description_p = "Deploy the latest version to production environment"
            $0.status = .pending
            $0.priority = .critical
            $0.tags = ["deployment", "production"]
            $0.assignedTo = "devops"
            $0.createdAt = now
            $0.updatedAt = now
        }
        
        tasks[task1.id] = task1
        tasks[task2.id] = task2
        tasks[task3.id] = task3
    }
}

/* 
// Note: This implementation requires proto file generation
// The following code will work after running protoc with Swift gRPC plugin

extension TaskServiceProvider: Task_TaskServiceAsyncProvider {
    func listTasks(
        request: Task_ListTasksRequest,
        context: GRPCAsyncServerCallContext
    ) -> GRPCAsyncResponseStream<Task_Task> {
        return GRPCAsyncResponseStream { writer in
            await queue.sync {
                // Filter tasks
                var filteredTasks = Array(tasks.values)
                
                if request.status != .unspecified {
                    filteredTasks = filteredTasks.filter { $0.status == request.status }
                }
                
                if !request.assignedTo.isEmpty {
                    filteredTasks = filteredTasks.filter { $0.assignedTo == request.assignedTo }
                }
                
                if !request.tags.isEmpty {
                    filteredTasks = filteredTasks.filter { task in
                        Set(request.tags).isSubset(of: Set(task.tags))
                    }
                }
                
                // Sort tasks
                switch request.sortOrder {
                case .createdDesc:
                    filteredTasks.sort { $0.createdAt.seconds > $1.createdAt.seconds }
                case .updatedDesc:
                    filteredTasks.sort { $0.updatedAt.seconds > $1.updatedAt.seconds }
                case .priorityDesc:
                    filteredTasks.sort { 
                        if $0.priority.rawValue != $1.priority.rawValue {
                            return $0.priority.rawValue > $1.priority.rawValue
                        }
                        return $0.createdAt.seconds < $1.createdAt.seconds
                    }
                default:
                    filteredTasks.sort { $0.createdAt.seconds < $1.createdAt.seconds }
                }
                
                // Apply pagination
                let pageSize = request.pageSize > 0 ? min(Int(request.pageSize), 100) : 20
                let startIndex = request.pageToken.isEmpty ? 0 : (Int(request.pageToken) ?? 0)
                let endIndex = min(startIndex + pageSize, filteredTasks.count)
                
                if startIndex < filteredTasks.count {
                    let paginatedTasks = Array(filteredTasks[startIndex..<endIndex])
                    
                    for task in paginatedTasks {
                        try await writer.send(task)
                    }
                }
            }
        }
    }
    
    func getTask(
        request: Task_GetTaskRequest,
        context: GRPCAsyncServerCallContext
    ) async throws -> Task_Task {
        return try await queue.sync {
            guard let task = tasks[request.id] else {
                throw GRPCStatus(code: .notFound, message: "Task with ID \(request.id) not found")
            }
            return task
        }
    }
    
    func createTask(
        request: Task_CreateTaskRequest,
        context: GRPCAsyncServerCallContext
    ) async throws -> Task_Task {
        return try await queue.sync {
            var task = request.task
            
            // Validate
            guard !task.title.trimmingCharacters(in: .whitespacesAndNewlines).isEmpty else {
                throw GRPCStatus(code: .invalidArgument, message: "Title is required")
            }
            
            guard task.title.count <= 200 else {
                throw GRPCStatus(code: .invalidArgument, message: "Title must be 200 characters or less")
            }
            
            // Generate ID and timestamps
            task.id = UUID().uuidString
            let now = Google_Protobuf_Timestamp.with {
                $0.seconds = Int64(Date().timeIntervalSince1970)
                $0.nanos = 0
            }
            task.createdAt = now
            task.updatedAt = now
            
            // Set defaults
            if task.createdBy.isEmpty {
                task.createdBy = "system"
            }
            
            tasks[task.id] = task
            return task
        }
    }
    
    func updateTask(
        request: Task_UpdateTaskRequest,
        context: GRPCAsyncServerCallContext
    ) async throws -> Task_Task {
        return try await queue.sync {
            let taskID = request.task.id
            
            guard !taskID.isEmpty else {
                throw GRPCStatus(code: .invalidArgument, message: "Task ID is required")
            }
            
            guard var existingTask = tasks[taskID] else {
                throw GRPCStatus(code: .notFound, message: "Task with ID \(taskID) not found")
            }
            
            // Apply updates based on update mask
            for field in request.updateMask {
                switch field {
                case "title":
                    existingTask.title = request.task.title
                case "description":
                    existingTask.description_p = request.task.description_p
                case "status":
                    existingTask.status = request.task.status
                case "priority":
                    existingTask.priority = request.task.priority
                case "tags":
                    existingTask.tags = request.task.tags
                case "assigned_to":
                    existingTask.assignedTo = request.task.assignedTo
                case "due_date":
                    existingTask.dueDate = request.task.dueDate
                default:
                    break
                }
            }
            
            // Update timestamp
            existingTask.updatedAt = Google_Protobuf_Timestamp.with {
                $0.seconds = Int64(Date().timeIntervalSince1970)
                $0.nanos = 0
            }
            
            tasks[taskID] = existingTask
            return existingTask
        }
    }
    
    func deleteTask(
        request: Task_DeleteTaskRequest,
        context: GRPCAsyncServerCallContext
    ) async throws -> Google_Protobuf_Empty {
        try await queue.sync {
            guard tasks.removeValue(forKey: request.id) != nil else {
                throw GRPCStatus(code: .notFound, message: "Task with ID \(request.id) not found")
            }
            return Google_Protobuf_Empty()
        }
    }
    
    func watchTasks(
        requestStream: GRPCAsyncRequestStream<Task_WatchTasksRequest>,
        context: GRPCAsyncServerCallContext
    ) -> GRPCAsyncResponseStream<Task_TaskEvent> {
        return GRPCAsyncResponseStream { writer in
            for try await request in requestStream {
                let now = Google_Protobuf_Timestamp.with {
                    $0.seconds = Int64(Date().timeIntervalSince1970)
                    $0.nanos = 0
                }
                
                if request.watchAll {
                    // Return all tasks as events
                    for task in tasks.values {
                        let event = Task_TaskEvent.with {
                            $0.eventType = .updated
                            $0.task = task
                            $0.timestamp = now
                        }
                        try await writer.send(event)
                    }
                } else if !request.taskIds.isEmpty {
                    // Return specific tasks
                    for taskID in request.taskIds {
                        if let task = tasks[taskID] {
                            let event = Task_TaskEvent.with {
                                $0.eventType = .updated
                                $0.task = task
                                $0.timestamp = now
                            }
                            try await writer.send(event)
                        }
                    }
                } else if !request.assignedTo.isEmpty {
                    // Return tasks assigned to specific user
                    for task in tasks.values where task.assignedTo == request.assignedTo {
                        let event = Task_TaskEvent.with {
                            $0.eventType = .updated
                            $0.task = task
                            $0.timestamp = now
                        }
                        try await writer.send(event)
                    }
                }
            }
        }
    }
}
*/

@main
struct TaskGrpcServerApp: AsyncParsableCommand {
    static let configuration = CommandConfiguration(
        commandName: "task-grpc-server",
        abstract: "A Swift gRPC server for the Task Management API"
    )
    
    @Option(name: .long, help: "Port to bind to")
    var port: Int = 50051
    
    @Option(name: .long, help: "Host to bind to")
    var host: String = "0.0.0.0"
    
    func run() async throws {
        print("Note: This is a template implementation.")
        print("To use this server, you need to:")
        print("1. Generate Swift protobuf files from tasks.proto")
        print("2. Add the generated files to the Generated target")
        print("3. Uncomment the service implementation above")
        print("")
        print("Run the following command to generate Swift files:")
        print("protoc --swift_out=Sources/Generated --grpc-swift_out=Sources/Generated ../../../shared/protos/tasks.proto")
        print("")
        
        /* 
        // Uncomment after proto generation:
        
        let group = MultiThreadedEventLoopGroup(numberOfThreads: 1)
        defer {
            try! group.syncShutdownGracefully()
        }
        
        let provider = TaskServiceProvider()
        
        let server = try await Server.insecure(group: group)
            .withServiceProviders([provider])
            .bind(host: host, port: port)
        
        print("Swift gRPC server started on \(host):\(port)")
        
        try await server.onClose.get()
        */
    }
}