module TaskRestServer.Database

open System
open System.Text.Json
open Microsoft.EntityFrameworkCore
open TaskRestServer.Models

type TaskDbContext(options: DbContextOptions<TaskDbContext>) =
    inherit DbContext(options)
    
    [<DefaultValue>]
    val mutable private _tasks: DbSet<Task>
    member this.Tasks 
        with get() = this._tasks
        and set v = this._tasks <- v
    
    override this.OnModelCreating(modelBuilder: ModelBuilder) =
        base.OnModelCreating(modelBuilder)
        
        modelBuilder.Entity<Task>(fun entity ->
            entity.HasKey(fun e -> e.Id :> obj) |> ignore
            entity.Property(fun e -> e.Id).HasMaxLength(36) |> ignore
            
            entity.Property(fun e -> e.Title)
                .IsRequired()
                .HasMaxLength(200) |> ignore
                
            entity.Property(fun e -> e.Description)
                .HasMaxLength(1000) |> ignore
                
            entity.Property(fun e -> e.Status)
                .HasConversion<string>() |> ignore
                
            entity.Property(fun e -> e.Priority)
                .HasConversion<string>() |> ignore
                
            // Convert F# list to JSON string for storage
            entity.Property(fun e -> e.Tags)
                .HasConversion(
                    (fun (tags: string list) -> JsonSerializer.Serialize(tags)),
                    (fun (json: string) -> 
                        match JsonSerializer.Deserialize<string list>(json) with
                        | null -> []
                        | tags -> tags)
                ) |> ignore
                
            entity.Property(fun e -> e.AssignedTo)
                .HasMaxLength(100) |> ignore
                
            entity.Property(fun e -> e.CreatedBy)
                .IsRequired()
                .HasMaxLength(100) |> ignore
                
            entity.Property(fun e -> e.CreatedAt)
                .HasDefaultValueSql("datetime('now')") |> ignore
                
            entity.Property(fun e -> e.UpdatedAt)
                .HasDefaultValueSql("datetime('now')") |> ignore
        ) |> ignore

module DatabaseHelpers =
    let createSampleTasks () =
        let now = DateTime.UtcNow
        
        [
            {
                Id = Guid.NewGuid().ToString()
                Title = "Complete F# project documentation"
                Description = Some "Write comprehensive documentation for the F# REST API"
                Status = TaskStatus.InProgress
                Priority = TaskPriority.High
                Tags = ["documentation"; "fsharp"; "api"]
                AssignedTo = Some "dev-team"
                CreatedBy = "system"
                CreatedAt = now
                UpdatedAt = now
                DueDate = None
            }
            {
                Id = Guid.NewGuid().ToString()
                Title = "Review functional programming patterns"
                Description = Some "Review and approve functional programming patterns in codebase"
                Status = TaskStatus.Pending
                Priority = TaskPriority.Medium
                Tags = ["review"; "functional"; "patterns"]
                AssignedTo = Some "senior-dev"
                CreatedBy = "system"
                CreatedAt = now
                UpdatedAt = now
                DueDate = None
            }
            {
                Id = Guid.NewGuid().ToString()
                Title = "Deploy F# services to production"
                Description = Some "Deploy the F# microservices to production environment"
                Status = TaskStatus.Pending
                Priority = TaskPriority.Critical
                Tags = ["deployment"; "production"; "fsharp"]
                AssignedTo = Some "devops"
                CreatedBy = "system"
                CreatedAt = now
                UpdatedAt = now
                DueDate = None
            }
        ]
    
    let initializeDatabase (context: TaskDbContext) =
        async {
            do! context.Database.EnsureCreatedAsync() |> Async.AwaitTask
            
            let! existingCount = 
                context.Tasks.CountAsync() 
                |> Async.AwaitTask
            
            if existingCount = 0 then
                let sampleTasks = createSampleTasks ()
                context.Tasks.AddRange(sampleTasks)
                let! _ = context.SaveChangesAsync() |> Async.AwaitTask
                ()
        }