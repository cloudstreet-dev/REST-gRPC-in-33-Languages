# C REST Client Makefile

CC = gcc
CFLAGS = -Wall -Wextra -O2
LDFLAGS = 

# Source and build directories
SRCDIR = src
OBJDIR = obj
BINDIR = .

# Source files
SOURCES = $(wildcard $(SRCDIR)/*.c)
OBJECTS = $(patsubst $(SRCDIR)/%.c,$(OBJDIR)/%.o,$(SOURCES))

# Target executable
TARGET = $(BINDIR)/task-rest-client

# Create directories
$(shell mkdir -p $(OBJDIR))

# Default target
all: $(TARGET)

# Build executable
$(TARGET): $(OBJECTS)
	@echo "Linking $@..."
	$(CC) $(LDFLAGS) $(OBJECTS) -o $@
	@echo "Build complete: $@"

# Compile C files
$(OBJDIR)/%.o: $(SRCDIR)/%.c
	@echo "Compiling $<..."
	$(CC) $(CFLAGS) -c $< -o $@

# Run the client
run: $(TARGET)
	./$(TARGET) help

# Run demo
demo: $(TARGET)
	./$(TARGET) demo

# Test commands
test: $(TARGET)
	@echo "Testing client commands..."
	./$(TARGET) health
	./$(TARGET) list

# Clean build artifacts
clean:
	@echo "Cleaning..."
	rm -rf $(OBJDIR) $(TARGET)
	@echo "Clean complete"

# Rebuild
rebuild: clean all

# Debug build
debug: CFLAGS += -g -DDEBUG
debug: clean all

# Help
help:
	@echo "C REST Client Makefile"
	@echo ""
	@echo "Targets:"
	@echo "  all     - Build the client (default)"
	@echo "  run     - Build and show help"
	@echo "  demo    - Run the demo"
	@echo "  test    - Test basic commands"
	@echo "  clean   - Remove build artifacts"
	@echo "  rebuild - Clean and rebuild"
	@echo "  debug   - Build with debug symbols"
	@echo "  help    - Show this help message"
	@echo ""
	@echo "Usage:"
	@echo "  make              # Build the client"
	@echo "  make demo         # Run the demo"
	@echo "  ./task-rest-client help  # Show client help"

.PHONY: all run demo test clean rebuild debug help