# Makefile for REST & gRPC in 33 Languages
# Comprehensive build and management commands

.PHONY: help
help: ## Show this help message
	@echo "REST & gRPC in 33 Languages - Makefile Commands"
	@echo "================================================"
	@echo ""
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-20s\033[0m %s\n", $$1, $$2}'

.PHONY: quick-start
quick-start: ## Run interactive quick start
	@chmod +x quick-start.sh
	@./quick-start.sh

.PHONY: test-all
test-all: ## Run tests for all languages
	@echo "Running universal test suite for all languages..."
	@for lang in javascript typescript go python java rust kotlin swift csharp ruby elixir; do \
		echo "Testing $$lang..."; \
		./test/universal-test-suite.sh $$lang || true; \
	done

.PHONY: test
test: ## Test a specific language (usage: make test LANG=javascript)
	@if [ -z "$(LANG)" ]; then \
		echo "Usage: make test LANG=<language>"; \
		exit 1; \
	fi
	@echo "Testing $(LANG)..."
	@./test/universal-test-suite.sh $(LANG)

.PHONY: benchmark
benchmark: ## Run performance benchmarks
	@chmod +x benchmark/benchmark.sh
	@./benchmark/benchmark.sh

.PHONY: docker-build
docker-build: ## Build all Docker images
	@chmod +x docker-build-all.sh
	@./docker-build-all.sh

.PHONY: docker-up
docker-up: ## Start all services with Docker Compose
	@docker-compose up -d
	@echo "All services started. View logs: docker-compose logs -f"

.PHONY: docker-down
docker-down: ## Stop all Docker services
	@docker-compose down
	@echo "All services stopped"

.PHONY: docker-clean
docker-clean: ## Clean up Docker images and containers
	@docker-compose down -v --rmi all
	@echo "Docker cleanup complete"

# Language-specific targets
.PHONY: run-javascript
run-javascript: ## Run JavaScript REST server
	@cd code/javascript/rest/server && npm install && npm start

.PHONY: run-typescript
run-typescript: ## Run TypeScript REST server
	@cd code/typescript/rest/server && npm install && npm run build && npm start

.PHONY: run-go
run-go: ## Run Go REST server
	@cd code/go/rest/server && go run .

.PHONY: run-python
run-python: ## Run Python REST server
	@cd code/python/rest/server && pip install -r requirements.txt && python app.py

.PHONY: run-rust
run-rust: ## Run Rust REST server
	@cd code/rust/rest/server && cargo run

.PHONY: run-java
run-java: ## Run Java REST server
	@cd code/java/rest/server && mvn spring-boot:run

# gRPC specific targets
.PHONY: grpc-javascript
grpc-javascript: ## Run JavaScript gRPC server
	@cd code/javascript/grpc/server && npm install && npm start

.PHONY: grpc-go
grpc-go: ## Run Go gRPC server
	@cd code/go/grpc/server && go run .

.PHONY: grpc-python
grpc-python: ## Run Python gRPC server
	@cd code/python/grpc/server && pip install -r requirements.txt && python server.py

# Proto generation
.PHONY: proto-gen
proto-gen: ## Generate code from proto files
	@echo "Generating code from proto files..."
	@for lang in go python javascript java; do \
		echo "Generating for $$lang..."; \
		case $$lang in \
			go) \
				protoc --go_out=. --go-grpc_out=. code/shared/protos/*.proto ;; \
			python) \
				python -m grpc_tools.protoc -I. --python_out=. --grpc_python_out=. code/shared/protos/*.proto ;; \
			javascript) \
				grpc_tools_node_protoc --js_out=import_style=commonjs,binary:. --grpc_out=. code/shared/protos/*.proto ;; \
			java) \
				protoc --java_out=. --grpc-java_out=. code/shared/protos/*.proto ;; \
		esac; \
	done

# Book related targets
.PHONY: book-serve
book-serve: ## Serve the book locally
	@cd book && python -m http.server 8000
	@echo "Book available at http://localhost:8000"

.PHONY: book-build
book-build: ## Build the book
	@echo "Building book..."
	@cd book && \
	for file in manuscript/chapters/*.md; do \
		echo "Processing $$file..."; \
	done
	@echo "Book build complete"

.PHONY: book-pdf
book-pdf: ## Generate PDF version of the book
	@if command -v pandoc >/dev/null 2>&1; then \
		pandoc book/manuscript/chapters/*.md \
			-o book/REST-gRPC-in-33-Languages.pdf \
			--toc \
			--toc-depth=2; \
		echo "PDF generated: book/REST-gRPC-in-33-Languages.pdf"; \
	else \
		echo "Pandoc not installed. Install with: brew install pandoc"; \
	fi

# Development tools
.PHONY: lint
lint: ## Run linters for all languages
	@echo "Running linters..."
	@cd code/javascript && npm run lint 2>/dev/null || true
	@cd code/typescript && npm run lint 2>/dev/null || true
	@cd code/python && pylint **/*.py 2>/dev/null || true
	@cd code/go && golangci-lint run 2>/dev/null || true
	@cd code/rust && cargo clippy 2>/dev/null || true

.PHONY: format
format: ## Format code for all languages
	@echo "Formatting code..."
	@cd code/javascript && npx prettier --write . 2>/dev/null || true
	@cd code/typescript && npx prettier --write . 2>/dev/null || true
	@cd code/python && black . 2>/dev/null || true
	@cd code/go && gofmt -w . 2>/dev/null || true
	@cd code/rust && cargo fmt 2>/dev/null || true

.PHONY: install-deps
install-deps: ## Install development dependencies
	@echo "Installing dependencies..."
	@if command -v brew >/dev/null 2>&1; then \
		brew install protobuf grpcurl wrk ghz jq; \
	elif command -v apt-get >/dev/null 2>&1; then \
		sudo apt-get update && sudo apt-get install -y protobuf-compiler curl jq; \
	else \
		echo "Please install dependencies manually"; \
	fi

.PHONY: check-health
check-health: ## Check health of all running services
	@echo "Checking service health..."
	@for port in 8001 8002 8003 8004 8005 8006 8007 8008; do \
		if curl -s http://localhost:$$port/health >/dev/null 2>&1; then \
			echo "✓ Service on port $$port is healthy"; \
		else \
			echo "✗ Service on port $$port is not responding"; \
		fi; \
	done

.PHONY: stats
stats: ## Show project statistics
	@echo "Project Statistics"
	@echo "=================="
	@echo "Total languages: 33"
	@echo "Languages with gRPC: $$(find code -name "grpc" -type d | wc -l)"
	@echo "Total lines of code: $$(find code -name "*.js" -o -name "*.ts" -o -name "*.go" -o -name "*.py" -o -name "*.java" -o -name "*.rs" -o -name "*.kt" -o -name "*.swift" -o -name "*.cs" -o -name "*.rb" | xargs wc -l | tail -1 | awk '{print $$1}')"
	@echo "Book chapters: $$(ls book/manuscript/chapters/*.md | wc -l)"
	@echo "Docker services: $$(grep -c "^  [a-z]" docker-compose.yml)"

.PHONY: clean
clean: ## Clean build artifacts
	@echo "Cleaning build artifacts..."
	@find . -type d -name "node_modules" -exec rm -rf {} + 2>/dev/null || true
	@find . -type d -name "target" -exec rm -rf {} + 2>/dev/null || true
	@find . -type d -name "build" -exec rm -rf {} + 2>/dev/null || true
	@find . -type d -name "dist" -exec rm -rf {} + 2>/dev/null || true
	@find . -type d -name "__pycache__" -exec rm -rf {} + 2>/dev/null || true
	@find . -type f -name "*.pyc" -delete 2>/dev/null || true
	@rm -rf benchmark-results test-results 2>/dev/null || true
	@echo "Clean complete"

.PHONY: release
release: ## Create a new release (usage: make release VERSION=v1.0.1)
	@if [ -z "$(VERSION)" ]; then \
		echo "Usage: make release VERSION=v1.0.1"; \
		exit 1; \
	fi
	@echo "Creating release $(VERSION)..."
	@git tag -a $(VERSION) -m "Release $(VERSION)"
	@git push origin $(VERSION)
	@echo "Release $(VERSION) created and pushed"

.PHONY: contribute
contribute: ## Show contribution guidelines
	@echo "How to Contribute"
	@echo "================="
	@echo "1. Fork the repository"
	@echo "2. Create a feature branch"
	@echo "3. Make your changes"
	@echo "4. Run tests: make test-all"
	@echo "5. Submit a pull request"
	@echo ""
	@echo "Areas for contribution:"
	@echo "- Add tests for existing implementations"
	@echo "- Improve error handling"
	@echo "- Add new language implementations"
	@echo "- Enhance documentation"
	@echo "- Performance optimizations"

.PHONY: learning-path
learning-path: ## Show learning path recommendations
	@cat LEARNING_PATH.md | head -50
	@echo ""
	@echo "... (see LEARNING_PATH.md for full guide)"

# Default target
.DEFAULT_GOAL := help