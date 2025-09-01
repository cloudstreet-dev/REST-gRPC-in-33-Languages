import Foundation
import ArgumentParser

enum TaskStatus: String, Codable, CaseIterable {
    case pending = "PENDING"
    case inProgress = "IN_PROGRESS"
    case completed = "COMPLETED"
    case cancelled = "CANCELLED"
    case archived = "ARCHIVED"
}

enum TaskPriority: String, Codable, CaseIterable {
    case low = "LOW"
    case medium = "MEDIUM"
    case high = "HIGH"
    case critical = "CRITICAL"
}

struct Task: Codable {
    let id: String
    let title: String
    let description: String?
    let status: TaskStatus
    let priority: TaskPriority
    let tags: [String]
    let assignedTo: String?
    let dueDate: Date?
    let createdAt: Date
    let updatedAt: Date
    let createdBy: String?
    let updatedBy: String?
}

struct CreateTaskRequest: Codable {
    let title: String
    let description: String?
    let priority: TaskPriority?
    let tags: [String]?
    let assignedTo: String?
    let dueDate: Date?
}

struct UpdateTaskRequest: Codable {
    let title: String?
    let description: String?
    let status: TaskStatus?
    let priority: TaskPriority?
    let tags: [String]?
    let assignedTo: String?
    let dueDate: Date?
}

struct UpdateStatusRequest: Codable {
    let status: TaskStatus
}

struct TasksResponse: Codable {
    let items: [Task]
    let pageSize: Int
    let nextPageToken: String?
    let totalCount: Int
}

struct ErrorResponse: Codable {
    let error: String
    let message: String
}

class TaskApiClient {
    private let baseURL: URL
    private let session: URLSession
    private let encoder = JSONEncoder()
    private let decoder = JSONDecoder()
    
    init(baseURL: String) {
        self.baseURL = URL(string: baseURL)!
        
        let config = URLSessionConfiguration.default
        config.timeoutIntervalForRequest = 30
        config.timeoutIntervalForResource = 60
        self.session = URLSession(configuration: config)
        
        // Configure JSON date handling
        let formatter = DateFormatter()
        formatter.dateFormat = "yyyy-MM-dd'T'HH:mm:ss.SSSZ"
        formatter.locale = Locale(identifier: "en_US_POSIX")
        formatter.timeZone = TimeZone(secondsFromGMT: 0)
        
        encoder.dateEncodingStrategy = .formatted(formatter)
        decoder.dateDecodingStrategy = .formatted(formatter)
    }
    
    func listTasks(pageSize: Int = 20,
                   pageToken: String? = nil,
                   status: TaskStatus? = nil,
                   assignedTo: String? = nil,
                   tags: [String]? = nil,
                   sortOrder: String? = nil) async throws -> TasksResponse {
        
        var components = URLComponents(url: baseURL.appendingPathComponent("api/v1/tasks"), resolvingAgainstBaseURL: true)!
        var queryItems: [URLQueryItem] = [
            URLQueryItem(name: "pageSize", value: String(pageSize))
        ]
        
        if let pageToken = pageToken {
            queryItems.append(URLQueryItem(name: "pageToken", value: pageToken))
        }
        
        if let status = status {
            queryItems.append(URLQueryItem(name: "status", value: status.rawValue))
        }
        
        if let assignedTo = assignedTo {
            queryItems.append(URLQueryItem(name: "assignedTo", value: assignedTo))
        }
        
        if let tags = tags, !tags.isEmpty {
            queryItems.append(URLQueryItem(name: "tags", value: tags.joined(separator: ",")))
        }
        
        if let sortOrder = sortOrder {
            queryItems.append(URLQueryItem(name: "sortOrder", value: sortOrder))
        }
        
        components.queryItems = queryItems
        
        let request = URLRequest(url: components.url!)
        let (data, response) = try await session.data(for: request)
        
        guard let httpResponse = response as? HTTPURLResponse else {
            throw URLError(.badServerResponse)
        }
        
        guard httpResponse.statusCode == 200 else {
            throw URLError(.badServerResponse)
        }
        
        return try decoder.decode(TasksResponse.self, from: data)
    }
    
    func getTask(id: String) async throws -> Task? {
        let url = baseURL.appendingPathComponent("api/v1/tasks/\(id)")
        let request = URLRequest(url: url)
        
        do {
            let (data, response) = try await session.data(for: request)
            
            guard let httpResponse = response as? HTTPURLResponse else {
                throw URLError(.badServerResponse)
            }
            
            if httpResponse.statusCode == 404 {
                return nil
            }
            
            guard httpResponse.statusCode == 200 else {
                throw URLError(.badServerResponse)
            }
            
            return try decoder.decode(Task.self, from: data)
        } catch {
            if let urlError = error as? URLError, urlError.code == .badServerResponse {
                return nil
            }
            throw error
        }
    }
    
    func createTask(title: String,
                    description: String? = nil,
                    priority: TaskPriority = .medium,
                    tags: [String] = [],
                    assignedTo: String? = nil) async throws -> Task {
        
        let createRequest = CreateTaskRequest(
            title: title,
            description: description,
            priority: priority,
            tags: tags.isEmpty ? nil : tags,
            assignedTo: assignedTo,
            dueDate: nil
        )
        
        let url = baseURL.appendingPathComponent("api/v1/tasks")
        var request = URLRequest(url: url)
        request.httpMethod = "POST"
        request.setValue("application/json", forHTTPHeaderField: "Content-Type")
        request.httpBody = try encoder.encode(createRequest)
        
        let (data, response) = try await session.data(for: request)
        
        guard let httpResponse = response as? HTTPURLResponse else {
            throw URLError(.badServerResponse)
        }
        
        guard httpResponse.statusCode == 201 else {
            if let errorResponse = try? decoder.decode(ErrorResponse.self, from: data) {
                throw NSError(domain: "TaskAPIError", code: httpResponse.statusCode, userInfo: [
                    NSLocalizedDescriptionKey: errorResponse.message
                ])
            }
            throw URLError(.badServerResponse)
        }
        
        return try decoder.decode(Task.self, from: data)
    }
    
    func updateTask(id: String, updates: UpdateTaskRequest) async throws -> Task? {
        let url = baseURL.appendingPathComponent("api/v1/tasks/\(id)")
        var request = URLRequest(url: url)
        request.httpMethod = "PUT"
        request.setValue("application/json", forHTTPHeaderField: "Content-Type")
        request.httpBody = try encoder.encode(updates)
        
        do {
            let (data, response) = try await session.data(for: request)
            
            guard let httpResponse = response as? HTTPURLResponse else {
                throw URLError(.badServerResponse)
            }
            
            if httpResponse.statusCode == 404 {
                return nil
            }
            
            guard httpResponse.statusCode == 200 else {
                throw URLError(.badServerResponse)
            }
            
            return try decoder.decode(Task.self, from: data)
        } catch {
            if let urlError = error as? URLError, urlError.code == .badServerResponse {
                return nil
            }
            throw error
        }
    }
    
    func updateTaskStatus(id: String, status: TaskStatus) async throws -> Task? {
        let statusRequest = UpdateStatusRequest(status: status)
        
        let url = baseURL.appendingPathComponent("api/v1/tasks/\(id)/status")
        var request = URLRequest(url: url)
        request.httpMethod = "PATCH"
        request.setValue("application/json", forHTTPHeaderField: "Content-Type")
        request.httpBody = try encoder.encode(statusRequest)
        
        do {
            let (data, response) = try await session.data(for: request)
            
            guard let httpResponse = response as? HTTPURLResponse else {
                throw URLError(.badServerResponse)
            }
            
            if httpResponse.statusCode == 404 {
                return nil
            }
            
            guard httpResponse.statusCode == 200 else {
                throw URLError(.badServerResponse)
            }
            
            return try decoder.decode(Task.self, from: data)
        } catch {
            if let urlError = error as? URLError, urlError.code == .badServerResponse {
                return nil
            }
            throw error
        }
    }
    
    func deleteTask(id: String) async throws -> Bool {
        let url = baseURL.appendingPathComponent("api/v1/tasks/\(id)")
        var request = URLRequest(url: url)
        request.httpMethod = "DELETE"
        
        do {
            let (_, response) = try await session.data(for: request)
            
            guard let httpResponse = response as? HTTPURLResponse else {
                throw URLError(.badServerResponse)
            }
            
            return httpResponse.statusCode == 204
        } catch {
            return false
        }
    }
}

@main
struct TaskClientApp: AsyncParsableCommand {
    static let configuration = CommandConfiguration(
        commandName: "task-client",
        abstract: "A Swift REST client for the Task Management API"
    )
    
    @Option(name: .long, help: "Base URL of the API server")
    var baseUrl: String = "http://localhost:8080"
    
    func run() async throws {
        let client = TaskApiClient(baseURL: baseUrl)
        
        print("Swift Task API Client")
        print("===================")
        
        do {
            // Create a task
            print("\nCreating a new task...")
            let task = try await client.createTask(
                title: "Test Swift REST Client",
                description: "Testing the Swift REST client implementation",
                priority: .high,
                tags: ["test", "swift", "rest"],
                assignedTo: "dev-team"
            )
            print("Created task: \(task.id) - \(task.title)")
            
            // Get the task
            print("\nRetrieving task \(task.id)...")
            if let retrieved = try await client.getTask(id: task.id) {
                print("Retrieved task: \(retrieved.title) - Status: \(retrieved.status.rawValue)")
            }
            
            // Update task status
            print("\nUpdating task status to IN_PROGRESS...")
            if let updated = try await client.updateTaskStatus(id: task.id, status: .inProgress) {
                print("Updated task status: \(updated.status.rawValue)")
            }
            
            // List all tasks
            print("\nListing all tasks...")
            let tasks = try await client.listTasks(pageSize: 10, sortOrder: "created_desc")
            for t in tasks.items {
                print("[\(t.status.rawValue)] \(t.title) - \(t.id)")
            }
            print("Total tasks: \(tasks.totalCount)")
            
            // Update task details
            print("\nUpdating task details...")
            let updateRequest = UpdateTaskRequest(
                title: nil,
                description: "Updated: Testing the Swift REST client implementation with Vapor",
                status: nil,
                priority: .medium,
                tags: ["test", "swift", "rest", "vapor"],
                assignedTo: nil,
                dueDate: nil
            )
            if let updatedTask = try await client.updateTask(id: task.id, updates: updateRequest) {
                print("Updated task: \(updatedTask.description ?? "No description")")
            }
            
            // Delete the task
            print("\nDeleting task \(task.id)...")
            let deleted = try await client.deleteTask(id: task.id)
            print("Task deleted: \(deleted)")
            
        } catch {
            print("Error: \(error.localizedDescription)")
        }
    }
}