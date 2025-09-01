using Microsoft.EntityFrameworkCore;
using TaskRestServer.Data;
using System.Text.Json.Serialization;

var builder = WebApplication.CreateBuilder(args);

// Add services to the container
builder.Services.AddControllers()
    .AddJsonOptions(options =>
    {
        options.JsonSerializerOptions.Converters.Add(new JsonStringEnumConverter());
        options.JsonSerializerOptions.PropertyNamingPolicy = System.Text.Json.JsonNamingPolicy.SnakeCaseLower;
    });

// Configure Entity Framework with SQLite
builder.Services.AddDbContext<TaskDbContext>(options =>
    options.UseSqlite(builder.Configuration.GetConnectionString("DefaultConnection") ?? "Data Source=tasks.db"));

// Add CORS
builder.Services.AddCors(options =>
{
    options.AddPolicy("AllowAll", policy =>
    {
        policy.AllowAnyOrigin()
              .AllowAnyMethod()
              .AllowAnyHeader();
    });
});

// Add API documentation
builder.Services.AddEndpointsApiExplorer();
builder.Services.AddSwaggerGen(c =>
{
    c.SwaggerDoc("v1", new() { Title = "Task Management API", Version = "v1" });
    c.IncludeXmlComments(Path.Combine(AppContext.BaseDirectory, "TaskRestServer.xml"));
});

var app = builder.Build();

// Configure the HTTP request pipeline
if (app.Environment.IsDevelopment())
{
    app.UseSwagger();
    app.UseSwaggerUI();
}

app.UseCors("AllowAll");
app.UseRouting();

app.MapControllers();

// Ensure database is created and seeded
using (var scope = app.Services.CreateScope())
{
    var context = scope.ServiceProvider.GetRequiredService<TaskDbContext>();
    try
    {
        context.Database.EnsureCreated();
        
        // Seed data if database is empty
        if (!context.Tasks.Any())
        {
            var now = DateTime.UtcNow;
            
            context.Tasks.AddRange(
                new TaskRestServer.Models.Task
                {
                    Id = Guid.NewGuid().ToString(),
                    Title = "Complete project documentation",
                    Description = "Write comprehensive documentation for the REST API",
                    Status = TaskRestServer.Models.TaskStatus.InProgress,
                    Priority = TaskRestServer.Models.TaskPriority.High,
                    Tags = new List<string> { "documentation", "api" },
                    AssignedTo = "dev-team",
                    CreatedBy = "system",
                    CreatedAt = now,
                    UpdatedAt = now
                },
                new TaskRestServer.Models.Task
                {
                    Id = Guid.NewGuid().ToString(),
                    Title = "Review pull requests",
                    Description = "Review and approve pending pull requests",
                    Status = TaskRestServer.Models.TaskStatus.Pending,
                    Priority = TaskRestServer.Models.TaskPriority.Medium,
                    Tags = new List<string> { "review", "code" },
                    AssignedTo = "senior-dev",
                    CreatedBy = "system",
                    CreatedAt = now,
                    UpdatedAt = now
                },
                new TaskRestServer.Models.Task
                {
                    Id = Guid.NewGuid().ToString(),
                    Title = "Deploy to production",
                    Description = "Deploy the latest version to production environment",
                    Status = TaskRestServer.Models.TaskStatus.Pending,
                    Priority = TaskRestServer.Models.TaskPriority.Critical,
                    Tags = new List<string> { "deployment", "production" },
                    AssignedTo = "devops",
                    CreatedBy = "system",
                    CreatedAt = now,
                    UpdatedAt = now
                }
            );
            
            context.SaveChanges();
        }
    }
    catch (Exception ex)
    {
        var logger = scope.ServiceProvider.GetRequiredService<ILogger<Program>>();
        logger.LogError(ex, "An error occurred while creating/seeding the database");
    }
}

app.Logger.LogInformation("C# Task REST Server starting on http://localhost:8080");
app.Run("http://localhost:8080");