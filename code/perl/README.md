# Perl Task Management API

This directory contains REST and gRPC implementations of the Task Management API in Perl.

## Project Structure

```
perl/
├── rest/
│   ├── server/
│   │   ├── lib/          # Perl modules
│   │   └── server.pl     # REST server script
│   └── client/
│       └── client.pl     # REST client script
└── grpc/
    └── README.md         # gRPC implementation notes
```

## Prerequisites

### Installing Perl and Dependencies

#### macOS
```bash
# Perl comes pre-installed on macOS
# Install cpanminus for package management
curl -L https://cpanmin.us | perl - --sudo App::cpanminus

# Install required modules
cpanm Mojolicious UUID::Tiny LWP::UserAgent JSON Thread::Semaphore
```

#### Ubuntu/Debian
```bash
sudo apt-get update
sudo apt-get install perl cpanminus

# Install required modules
sudo cpanm Mojolicious UUID::Tiny LWP::UserAgent JSON Thread::Semaphore
```

#### Using Docker
```dockerfile
FROM perl:5.36
RUN cpanm Mojolicious UUID::Tiny LWP::UserAgent JSON Thread::Semaphore
```

## Running the Application

### REST Server

```bash
cd rest/server
perl server.pl

# Or make it executable
chmod +x server.pl
./server.pl

# Run on different port
PORT=8081 perl server.pl
```

### REST Client

```bash
cd rest/client
perl client.pl help

# Make it executable
chmod +x client.pl
./client.pl demo
```

## Implementation Details

### Perl Features Used

1. **Object-Oriented Programming**: Package-based OOP
2. **References**: Complex data structures
3. **Regular Expressions**: Built-in regex support
4. **CPAN Modules**: Rich ecosystem
5. **Thread Safety**: Semaphores for concurrency
6. **Hash Tables**: Native associative arrays
7. **List Processing**: map, grep, sort
8. **Dynamic Typing**: Flexible variable types

### Mojolicious Framework

Mojolicious is a modern Perl web framework featuring:
- **Real-time web framework**: WebSocket and EventSource support
- **RESTful routes**: Simple routing DSL
- **Built-in JSON support**: Automatic JSON parsing
- **Non-blocking I/O**: Event loop based on EV
- **Testing framework**: Built-in testing tools
- **Template system**: Embedded Perl templates

## API Endpoints

```
GET    /api/tasks          - List all tasks
GET    /api/tasks/{id}     - Get specific task
POST   /api/tasks          - Create new task
PUT    /api/tasks/{id}     - Update task
PATCH  /api/tasks/{id}/status - Update task status
DELETE /api/tasks/{id}     - Delete task
GET    /health             - Health check
```

## Client Usage

### Commands

```bash
# List all tasks
./client.pl list

# List with filters
./client.pl list --status pending --assigned-to backend-team

# Get specific task
./client.pl get <task-id>

# Create task
echo '{"title":"New Task","priority":"high"}' | ./client.pl create

# Update task
echo '{"title":"Updated Task"}' | ./client.pl update <task-id>

# Update status
./client.pl status <task-id> completed

# Delete task
./client.pl delete <task-id>

# Health check
./client.pl health

# Run demo
./client.pl demo
```

## Testing

```bash
# Start server
cd rest/server && perl server.pl &

# Run client tests
cd rest/client
perl client.pl demo

# Manual testing with curl
curl http://localhost:8080/api/tasks
curl -X POST http://localhost:8080/api/tasks \
  -H "Content-Type: application/json" \
  -d '{"title":"Test Task","priority":"high"}'

# Stop server
kill %1
```

## Perl Language Highlights

### Strengths
- **Text Processing**: Unmatched regex and string manipulation
- **CPAN**: Comprehensive Perl Archive Network
- **Rapid Development**: Concise syntax for common tasks
- **System Administration**: Excellent for scripting
- **Cross-platform**: Runs everywhere
- **Mature**: Battle-tested over decades

### Modern Perl Features
```perl
# Postfix conditionals
print "Hello" if $condition;

# Smart matching (Perl 5.10+)
given ($value) {
    when ('foo') { say "Got foo" }
    when ('bar') { say "Got bar" }
    default { say "Got something else" }
}

# State variables
use feature 'state';
sub counter {
    state $count = 0;
    return ++$count;
}

# Signatures (experimental)
use feature 'signatures';
sub add ($x, $y) {
    return $x + $y;
}
```

## Performance Considerations

- **Interpreted Language**: Slower than compiled languages
- **Memory Usage**: Higher than C/C++/Rust
- **Concurrency**: Thread support but processes preferred
- **Regular Expressions**: Highly optimized
- **String Operations**: Very efficient

## Production Deployment

### Plack/PSGI
For production, use Plack for better performance:

```bash
# Install Plack
cpanm Plack

# Run with Plack
plackup -s Starman --workers 10 -p 8080 server.psgi
```

### Systemd Service
```ini
[Unit]
Description=Perl Task API
After=network.target

[Service]
Type=simple
User=www-data
WorkingDirectory=/opt/task-api
ExecStart=/usr/bin/perl server.pl
Restart=always

[Install]
WantedBy=multi-user.target
```

## Comparison with Other Languages

### Perl vs Python
- **Perl**: Better text processing, more concise
- **Python**: Cleaner syntax, larger community

### Perl vs Ruby
- **Perl**: More mature, faster regex
- **Ruby**: More consistent OOP, Rails ecosystem

### Perl vs PHP
- **Perl**: Better for system administration
- **PHP**: Better web framework ecosystem

## Common Issues

### Module Installation
```bash
# If cpanm fails, try with force
cpanm --force Module::Name

# Or use system packages
sudo apt-get install libmojolicious-perl
```

### Unicode Issues
```perl
use utf8;
use open ':std', ':encoding(UTF-8)';
```

### Memory Leaks
```perl
# Use weak references for circular references
use Scalar::Util 'weaken';
weaken($self->{parent});
```

## Resources

- [Perl Documentation](https://perldoc.perl.org/)
- [Mojolicious Documentation](https://docs.mojolicious.org/)
- [CPAN](https://metacpan.org/)
- [Modern Perl Book](http://modernperlbooks.com/)
- [Perl Weekly](https://perlweekly.com/)

## License

This implementation is part of the "REST APIs and gRPC in 33 Languages" project.