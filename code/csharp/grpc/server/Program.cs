using TaskGrpcServer.Services;

var builder = WebApplication.CreateBuilder(args);

// Add gRPC services
builder.Services.AddGrpc();

// Add logging
builder.Logging.ClearProviders();
builder.Logging.AddConsole();

var app = builder.Build();

// Configure the HTTP request pipeline
app.MapGrpcService<TaskService>();

app.MapGet("/", () => 
    "Communication with gRPC endpoints must be made through a gRPC client. " +
    "To learn how to create a client, visit: https://go.microsoft.com/fwlink/?linkid=2086909");

app.Logger.LogInformation("C# gRPC server starting on http://localhost:50051");

app.Run("http://localhost:50051");