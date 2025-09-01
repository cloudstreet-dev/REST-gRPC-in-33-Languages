package main

import (
	"fmt"
	"log"
	"strings"

	"github.com/cloudstreet-dev/REST-gRPC-in-33-Languages/code/go/internal/models"
	"github.com/cloudstreet-dev/REST-gRPC-in-33-Languages/code/go/rest/client"
)

func main() {
	// Create client
	apiClient := client.NewTaskAPIClient("")

	fmt.Println("=== Go REST Client Demo ===\n")

	// Create a task
	fmt.Println("Creating task...")
	newTask, err := apiClient.CreateTask(models.CreateTaskRequest{
		Title:       "Learn Go REST APIs",
		Description: "Complete Chapter 4 of the book",
		Priority:    models.TaskPriorityHigh,
		Tags:        []string{"learning", "go", "api"},
	})
	if err != nil {
		log.Fatalf("Failed to create task: %v", err)
	}
	fmt.Printf("Created task: %s (ID: %s)\n\n", newTask.Title, newTask.ID)

	// List all tasks
	fmt.Println("Listing tasks...")
	listResp, err := apiClient.ListTasks(models.ListTasksQuery{
		Status:    models.TaskStatusPending,
		SortOrder: models.SortOrderPriorityDesc,
	})
	if err != nil {
		log.Fatalf("Failed to list tasks: %v", err)
	}
	fmt.Printf("Found %d pending tasks:\n", len(listResp.Tasks))
	for _, task := range listResp.Tasks {
		fmt.Printf("  - %s (Priority: %s)\n", task.Title, task.Priority)
	}
	fmt.Println()

	// Get single task
	fmt.Printf("Getting task %s...\n", newTask.ID)
	retrieved, err := apiClient.GetTask(newTask.ID)
	if err != nil {
		log.Fatalf("Failed to get task: %v", err)
	}
	fmt.Printf("Retrieved: %s\n\n", retrieved.Title)

	// Update task status
	fmt.Println("Updating task status...")
	updated, err := apiClient.UpdateTaskStatus(newTask.ID, models.TaskStatusInProgress)
	if err != nil {
		log.Fatalf("Failed to update task status: %v", err)
	}
	fmt.Printf("Updated status: %s\n\n", updated.Status)

	// Update task details
	fmt.Println("Updating task details...")
	newDesc := "Updated description"
	fullyUpdated, err := apiClient.UpdateTask(newTask.ID, models.UpdateTaskRequest{
		Description: &newDesc,
		Tags:        []string{"go", "updated"},
	})
	if err != nil {
		log.Fatalf("Failed to update task: %v", err)
	}
	fmt.Printf("Updated tags: %s\n\n", strings.Join(fullyUpdated.Tags, ", "))

	// Delete task
	fmt.Println("Deleting task...")
	err = apiClient.DeleteTask(newTask.ID)
	if err != nil {
		log.Fatalf("Failed to delete task: %v", err)
	}
	fmt.Println("Task deleted successfully")

	fmt.Println("\n✅ All REST API operations completed successfully!")
}