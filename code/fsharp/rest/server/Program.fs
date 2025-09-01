open System
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.Logging
open Microsoft.EntityFrameworkCore
open System.Text.Json.Serialization
open TaskRestServer.Database
open TaskRestServer.Controllers

[<EntryPoint>]
let main args =
    let builder = WebApplication.CreateBuilder(args)

    // Add services to the container
    builder.Services.AddControllers()
        .AddJsonOptions(fun options ->
            options.JsonSerializerOptions.Converters.Add(JsonStringEnumConverter())
            options.JsonSerializerOptions.PropertyNamingPolicy <- System.Text.Json.JsonNamingPolicy.SnakeCaseLower
        ) |> ignore

    // Configure Entity Framework with SQLite
    let connectionString = 
        match builder.Configuration.GetConnectionString("DefaultConnection") with
        | null -> "Data Source=tasks.db"
        | conn -> conn
        
    builder.Services.AddDbContext<TaskDbContext>(fun options ->
        options.UseSqlite(connectionString) |> ignore
    ) |> ignore

    // Add CORS
    builder.Services.AddCors(fun options ->
        options.AddPolicy("AllowAll", fun policy ->
            policy.AllowAnyOrigin()
                  .AllowAnyMethod()
                  .AllowAnyHeader() |> ignore
        )
    ) |> ignore

    // Add API documentation
    builder.Services.AddEndpointsApiExplorer() |> ignore
    builder.Services.AddSwaggerGen(fun c ->
        c.SwaggerDoc("v1", Microsoft.OpenApi.Models.OpenApiInfo(Title = "F# Task Management API", Version = "v1"))
    ) |> ignore

    let app = builder.Build()

    // Configure the HTTP request pipeline
    if app.Environment.IsDevelopment() then
        app.UseSwagger() |> ignore
        app.UseSwaggerUI() |> ignore

    app.UseCors("AllowAll") |> ignore
    app.UseRouting() |> ignore
    app.MapControllers() |> ignore

    // Initialize database with sample data
    use scope = app.Services.CreateScope()
    let context = scope.ServiceProvider.GetRequiredService<TaskDbContext>()
    let logger = scope.ServiceProvider.GetRequiredService<ILogger<TaskDbContext>>()
    
    try
        DatabaseHelpers.initializeDatabase context
        |> Async.RunSynchronously
        logger.LogInformation("Database initialized successfully")
    with
    | ex -> 
        logger.LogError(ex, "An error occurred while initializing the database")

    printfn "F# Task REST Server starting on http://localhost:8080"
    app.Run("http://localhost:8080")
    
    0