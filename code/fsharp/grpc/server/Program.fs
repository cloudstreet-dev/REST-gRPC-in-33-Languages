open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.Logging
open TaskGrpcServer.TaskService

[<EntryPoint>]
let main args =
    let builder = WebApplication.CreateBuilder(args)

    // Add gRPC services
    builder.Services.AddGrpc() |> ignore

    // Add logging
    builder.Logging.ClearProviders() |> ignore
    builder.Logging.AddConsole() |> ignore

    let app = builder.Build()

    // Configure the HTTP request pipeline
    app.MapGrpcService<TaskServiceImplementation>() |> ignore

    app.MapGet("/", fun () -> 
        "Communication with gRPC endpoints must be made through a gRPC client. " +
        "To learn how to create a client, visit: https://go.microsoft.com/fwlink/?linkid=2086909"
    ) |> ignore

    let logger = app.Services.GetRequiredService<ILogger<TaskServiceImplementation>>()
    logger.LogInformation("F# gRPC server starting on http://localhost:50051")

    app.Run("http://localhost:50051")
    
    0