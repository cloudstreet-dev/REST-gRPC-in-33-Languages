using Microsoft.EntityFrameworkCore;
using TaskRestServer.Models;
using System.Text.Json;

namespace TaskRestServer.Data;

public class TaskDbContext : DbContext
{
    public TaskDbContext(DbContextOptions<TaskDbContext> options) : base(options)
    {
    }

    public DbSet<Models.Task> Tasks { get; set; }

    protected override void OnModelCreating(ModelBuilder modelBuilder)
    {
        base.OnModelCreating(modelBuilder);

        modelBuilder.Entity<Models.Task>(entity =>
        {
            entity.HasKey(e => e.Id);
            entity.Property(e => e.Id).HasMaxLength(36);
            
            entity.Property(e => e.Title)
                .IsRequired()
                .HasMaxLength(200);
                
            entity.Property(e => e.Description)
                .HasMaxLength(1000);
                
            entity.Property(e => e.Status)
                .HasConversion<string>();
                
            entity.Property(e => e.Priority)
                .HasConversion<string>();
                
            entity.Property(e => e.Tags)
                .HasConversion(
                    v => JsonSerializer.Serialize(v, (JsonSerializerOptions?)null),
                    v => JsonSerializer.Deserialize<List<string>>(v, (JsonSerializerOptions?)null) ?? new List<string>()
                );
                
            entity.Property(e => e.AssignedTo)
                .HasMaxLength(100);
                
            entity.Property(e => e.CreatedBy)
                .IsRequired()
                .HasMaxLength(100);
                
            entity.Property(e => e.CreatedAt)
                .HasDefaultValueSql("datetime('now')");
                
            entity.Property(e => e.UpdatedAt)
                .HasDefaultValueSql("datetime('now')");
        });

        // Seed data
        var now = DateTime.UtcNow;
        
        modelBuilder.Entity<Models.Task>().HasData(
            new Models.Task
            {
                Id = Guid.NewGuid().ToString(),
                Title = "Complete project documentation",
                Description = "Write comprehensive documentation for the REST API",
                Status = TaskStatus.InProgress,
                Priority = TaskPriority.High,
                Tags = new List<string> { "documentation", "api" },
                AssignedTo = "dev-team",
                CreatedBy = "system",
                CreatedAt = now,
                UpdatedAt = now
            },
            new Models.Task
            {
                Id = Guid.NewGuid().ToString(),
                Title = "Review pull requests",
                Description = "Review and approve pending pull requests",
                Status = TaskStatus.Pending,
                Priority = TaskPriority.Medium,
                Tags = new List<string> { "review", "code" },
                AssignedTo = "senior-dev",
                CreatedBy = "system",
                CreatedAt = now,
                UpdatedAt = now
            },
            new Models.Task
            {
                Id = Guid.NewGuid().ToString(),
                Title = "Deploy to production",
                Description = "Deploy the latest version to production environment",
                Status = TaskStatus.Pending,
                Priority = TaskPriority.Critical,
                Tags = new List<string> { "deployment", "production" },
                AssignedTo = "devops",
                CreatedBy = "system",
                CreatedAt = now,
                UpdatedAt = now
            }
        );
    }
}