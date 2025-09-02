# Makefile for Objective-C REST Server

CC = clang
OBJC = clang
CFLAGS = -Wall -Wextra -O2 -fobjc-arc
FRAMEWORKS = -framework Foundation -framework GCDWebServer
INCLUDES = -I./include -I/usr/local/include
LDFLAGS = -L/usr/local/lib

# Source files
SOURCES = src/main.m src/Task.m src/TaskRepository.m src/TaskServer.m
OBJECTS = $(SOURCES:.m=.o)
EXECUTABLE = task-rest-server

# Default target
all: $(EXECUTABLE)

$(EXECUTABLE): $(OBJECTS)
	$(OBJC) $(CFLAGS) $(FRAMEWORKS) $(LDFLAGS) -o $@ $^

%.o: %.m
	$(OBJC) $(CFLAGS) $(INCLUDES) -c $< -o $@

run: $(EXECUTABLE)
	./$(EXECUTABLE)

clean:
	rm -f $(OBJECTS) $(EXECUTABLE)

install-deps:
	@echo "Installing GCDWebServer..."
	@if [ ! -d "/usr/local/include/GCDWebServer" ]; then \
		git clone https://github.com/swisspol/GCDWebServer.git /tmp/GCDWebServer; \
		cd /tmp/GCDWebServer && make install; \
		rm -rf /tmp/GCDWebServer; \
	else \
		echo "GCDWebServer already installed"; \
	fi

.PHONY: all run clean install-deps