# C REST Server Makefile

CC = gcc
CFLAGS = -Wall -Wextra -O2 -pthread -I./include
LDFLAGS = -pthread

# Source and build directories
SRCDIR = src
INCDIR = include
OBJDIR = obj
BINDIR = .

# Source files
SOURCES = $(wildcard $(SRCDIR)/*.c)
OBJECTS = $(patsubst $(SRCDIR)/%.c,$(OBJDIR)/%.o,$(SOURCES))

# Target executable
TARGET = $(BINDIR)/task-rest-server

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

# Run the server
run: $(TARGET)
	./$(TARGET)

# Run with custom port
run-port: $(TARGET)
	./$(TARGET) 8081

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
	@echo "C REST Server Makefile"
	@echo ""
	@echo "Targets:"
	@echo "  all       - Build the server (default)"
	@echo "  run       - Build and run the server"
	@echo "  run-port  - Run on port 8081"
	@echo "  clean     - Remove build artifacts"
	@echo "  rebuild   - Clean and rebuild"
	@echo "  debug     - Build with debug symbols"
	@echo "  help      - Show this help message"
	@echo ""
	@echo "Usage:"
	@echo "  make              # Build the server"
	@echo "  make run          # Build and run"
	@echo "  ./task-rest-server [port]  # Run on specific port"

.PHONY: all run run-port clean rebuild debug help