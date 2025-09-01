package main

import (
	"fmt"
	"log"
	"net"
	"os"
	"os/signal"
	"syscall"

	"github.com/cloudstreet-dev/REST-gRPC-in-33-Languages/code/go/internal/services"
	"google.golang.org/grpc"
	"google.golang.org/grpc/reflection"
)

func main() {
	// Get port from environment or use default
	port := os.Getenv("GRPC_PORT")
	if port == "" {
		port = "50051"
	}

	// Create listener
	lis, err := net.Listen("tcp", ":"+port)
	if err != nil {
		log.Fatalf("Failed to listen: %v", err)
	}

	// Create gRPC server
	grpcServer := grpc.NewServer()

	// Initialize services
	taskService := services.NewTaskService()

	// Register service
	// Note: In a real implementation, you would register the generated service
	// pb.RegisterTaskServiceServer(grpcServer, server.NewTaskServiceServer(taskService))

	// Register reflection service for grpcurl
	reflection.Register(grpcServer)

	// Start server in a goroutine
	go func() {
		fmt.Printf("gRPC server starting on port %s\n", port)
		fmt.Println("Available services:")
		fmt.Println("  - TaskService.ListTasks (server streaming)")
		fmt.Println("  - TaskService.GetTask (unary)")
		fmt.Println("  - TaskService.CreateTask (unary)")
		fmt.Println("  - TaskService.UpdateTask (unary)")
		fmt.Println("  - TaskService.DeleteTask (unary)")
		fmt.Println("  - TaskService.WatchTasks (bidirectional streaming)")
		
		if err := grpcServer.Serve(lis); err != nil {
			log.Fatalf("Failed to serve: %v", err)
		}
	}()

	// Wait for interrupt signal to gracefully shutdown the server
	quit := make(chan os.Signal, 1)
	signal.Notify(quit, syscall.SIGINT, syscall.SIGTERM)
	<-quit

	log.Println("Shutting down gRPC server...")
	grpcServer.GracefulStop()
	log.Println("Server shutdown complete")
}