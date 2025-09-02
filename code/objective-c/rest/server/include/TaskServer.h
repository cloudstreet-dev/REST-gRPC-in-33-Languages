#import <Foundation/Foundation.h>

@interface TaskServer : NSObject

@property (nonatomic, readonly) BOOL isRunning;
@property (nonatomic, readonly) NSUInteger port;

// Server lifecycle
- (BOOL)startServerOnPort:(NSUInteger)port;
- (void)stopServer;

// Run loop (for command-line apps)
- (void)runUntilTerminated;

@end