package com.taskapi.grpc;

import io.grpc.Server;
import io.grpc.ServerBuilder;
import io.grpc.protobuf.services.ProtoReflectionService;
import lombok.extern.slf4j.Slf4j;
import java.io.IOException;
import java.util.concurrent.TimeUnit;

@Slf4j
public class TaskGrpcServer {
    private Server server;
    private final int port;
    
    public TaskGrpcServer(int port) {
        this.port = port;
    }
    
    public void start() throws IOException {
        server = ServerBuilder.forPort(port)
                .addService(new TaskServiceImpl())
                .addService(ProtoReflectionService.newInstance())
                .build()
                .start();
        
        log.info("Java gRPC server started on port {}", port);
        
        Runtime.getRuntime().addShutdownHook(new Thread(() -> {
            log.info("Shutting down gRPC server...");
            try {
                TaskGrpcServer.this.stop();
            } catch (InterruptedException e) {
                log.error("Error during shutdown", e);
            }
            log.info("Server shut down");
        }));
    }
    
    public void stop() throws InterruptedException {
        if (server != null) {
            server.shutdown().awaitTermination(30, TimeUnit.SECONDS);
        }
    }
    
    public void blockUntilShutdown() throws InterruptedException {
        if (server != null) {
            server.awaitTermination();
        }
    }
    
    public static void main(String[] args) throws IOException, InterruptedException {
        TaskGrpcServer server = new TaskGrpcServer(50051);
        server.start();
        server.blockUntilShutdown();
    }
}