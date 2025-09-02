#import <Foundation/Foundation.h>
#import "TaskServer.h"

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSLog(@"╔════════════════════════════════════════════════╗");
        NSLog(@"║     Objective-C Task Management REST API       ║");
        NSLog(@"║         Built with Foundation & GCDWebServer   ║");
        NSLog(@"╚════════════════════════════════════════════════╝");
        NSLog(@"");
        
        TaskServer *server = [[TaskServer alloc] init];
        
        NSUInteger port = 8080;
        if (argc > 1) {
            port = [[NSString stringWithUTF8String:argv[1]] integerValue];
            if (port == 0) {
                port = 8080;
            }
        }
        
        if ([server startServerOnPort:port]) {
            NSLog(@"");
            NSLog(@"Available endpoints:");
            NSLog(@"  GET    /api/tasks          - List all tasks");
            NSLog(@"  GET    /api/tasks/{id}     - Get a specific task");
            NSLog(@"  POST   /api/tasks          - Create a new task");
            NSLog(@"  PUT    /api/tasks/{id}     - Update a task");
            NSLog(@"  PATCH  /api/tasks/{id}/status - Update task status");
            NSLog(@"  DELETE /api/tasks/{id}     - Delete a task");
            NSLog(@"  GET    /health             - Health check");
            NSLog(@"");
            NSLog(@"Sample requests:");
            NSLog(@"  curl http://localhost:%lu/api/tasks", (unsigned long)port);
            NSLog(@"  curl -X POST http://localhost:%lu/api/tasks \\", (unsigned long)port);
            NSLog(@"    -H \"Content-Type: application/json\" \\");
            NSLog(@"    -d '{\"title\":\"New Task\",\"priority\":\"high\"}'");
            NSLog(@"");
            
            [server runUntilTerminated];
        } else {
            NSLog(@"Failed to start server on port %lu", (unsigned long)port);
            return 1;
        }
    }
    return 0;
}