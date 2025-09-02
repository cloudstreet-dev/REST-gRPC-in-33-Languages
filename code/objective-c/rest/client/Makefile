# Objective-C REST Client Makefile

# Compiler and flags
CC = clang
OBJC = clang
CFLAGS = -Wall -Wextra -O2
OBJCFLAGS = -fobjc-arc -framework Foundation $(CFLAGS)

# Directories
SRCDIR = src
INCDIR = include
BUILDDIR = build
BINDIR = bin

# Source files
SOURCES = $(wildcard $(SRCDIR)/*.m)
OBJECTS = $(patsubst $(SRCDIR)/%.m,$(BUILDDIR)/%.o,$(SOURCES))

# Executable
TARGET = $(BINDIR)/client

# Include paths
INCLUDES = -I$(INCDIR)

# Create directories
$(shell mkdir -p $(BUILDDIR) $(BINDIR))

# Default target
all: $(TARGET)

# Build executable
$(TARGET): $(OBJECTS)
	@echo "Linking $@..."
	$(OBJC) $(OBJCFLAGS) $(OBJECTS) -o $@
	@echo "Build complete: $@"

# Compile Objective-C files
$(BUILDDIR)/%.o: $(SRCDIR)/%.m
	@echo "Compiling $<..."
	$(OBJC) $(OBJCFLAGS) $(INCLUDES) -c $< -o $@

# Run the client
run: $(TARGET)
	$(TARGET)

# Run demo
demo: $(TARGET)
	$(TARGET) demo

# Run with custom server URL
run-custom: $(TARGET)
	TASK_API_URL=http://localhost:8080 $(TARGET) demo

# Clean build artifacts
clean:
	@echo "Cleaning..."
	rm -rf $(BUILDDIR) $(BINDIR)
	@echo "Clean complete"

# Rebuild
rebuild: clean all

# Show help
help:
	@echo "Objective-C REST Client Makefile"
	@echo ""
	@echo "Targets:"
	@echo "  all       - Build the client (default)"
	@echo "  run       - Build and run the client"
	@echo "  demo      - Run the demo"
	@echo "  clean     - Remove build artifacts"
	@echo "  rebuild   - Clean and rebuild"
	@echo "  help      - Show this help message"
	@echo ""
	@echo "Environment Variables:"
	@echo "  TASK_API_URL - Base URL for the Task API (default: http://localhost:8080)"
	@echo ""
	@echo "Usage Examples:"
	@echo "  make              # Build the client"
	@echo "  make demo         # Run the demo"
	@echo "  make clean        # Clean build artifacts"
	@echo "  ./bin/client help # Show client usage"

.PHONY: all run demo run-custom clean rebuild help