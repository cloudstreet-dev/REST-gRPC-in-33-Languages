#!/bin/bash

# Performance Benchmark Suite
# Compares REST and gRPC performance across languages

set -e

# Configuration
LANGUAGES=(javascript typescript go python java rust kotlin swift csharp fsharp cpp scala)
RESULTS_DIR="benchmark-results"
TIMESTAMP=$(date +%Y%m%d_%H%M%S)

# Create results directory
mkdir -p "$RESULTS_DIR"

# Benchmark parameters
CONCURRENCY=10
REQUESTS=1000
DURATION=30

echo "======================================"
echo "REST & gRPC Performance Benchmark"
echo "======================================"
echo "Concurrency: $CONCURRENCY"
echo "Requests: $REQUESTS"
echo "Duration: ${DURATION}s"
echo

# Function to benchmark REST API
benchmark_rest() {
    local LANG=$1
    local PORT=${2:-8080}
    local URL="http://localhost:${PORT}/api/v1/tasks"
    
    echo "Benchmarking REST API for $LANG..."
    
    # Using Apache Bench (ab)
    if command -v ab &> /dev/null; then
        ab -n $REQUESTS -c $CONCURRENCY -T application/json \
           -p benchmark/create_task.json \
           "$URL" > "$RESULTS_DIR/${LANG}_rest_${TIMESTAMP}.txt" 2>&1
    fi
    
    # Using wrk for more detailed metrics
    if command -v wrk &> /dev/null; then
        wrk -t4 -c${CONCURRENCY} -d${DURATION}s \
            -s benchmark/wrk_script.lua \
            --latency \
            "$URL" > "$RESULTS_DIR/${LANG}_rest_wrk_${TIMESTAMP}.txt" 2>&1
    fi
}

# Function to benchmark gRPC
benchmark_grpc() {
    local LANG=$1
    local PORT=${2:-50051}
    
    echo "Benchmarking gRPC for $LANG..."
    
    # Using ghz for gRPC benchmarking
    if command -v ghz &> /dev/null; then
        ghz --insecure \
            --proto code/shared/protos/tasks.proto \
            --call tasks.v1.TaskService.ListTasks \
            -d '{"page_size": 20}' \
            -c $CONCURRENCY \
            -n $REQUESTS \
            --connections=$CONCURRENCY \
            localhost:${PORT} > "$RESULTS_DIR/${LANG}_grpc_${TIMESTAMP}.json"
    fi
}

# Create test data file for POST requests
cat > benchmark/create_task.json <<EOF
{
    "title": "Benchmark Task",
    "description": "Performance testing task",
    "priority": "medium",
    "tags": ["benchmark", "test"]
}
EOF

# Create wrk Lua script for more complex scenarios
cat > benchmark/wrk_script.lua <<'EOF'
-- wrk script for testing REST API

local counter = 0
local threads = {}

function setup(thread)
    thread:set("id", counter)
    table.insert(threads, thread)
    counter = counter + 1
end

function init(args)
    requests = 0
end

function request()
    requests = requests + 1
    
    -- Alternate between different operations
    if requests % 4 == 0 then
        -- GET list
        return wrk.format("GET", "/api/v1/tasks")
    elseif requests % 4 == 1 then
        -- POST create
        wrk.method = "POST"
        wrk.headers["Content-Type"] = "application/json"
        wrk.body = '{"title":"Load Test Task","priority":"high"}'
        return wrk.format()
    elseif requests % 4 == 2 then
        -- GET single
        return wrk.format("GET", "/api/v1/tasks/task-1")
    else
        -- PUT update
        wrk.method = "PUT"
        wrk.headers["Content-Type"] = "application/json"
        wrk.body = '{"title":"Updated Task","status":"completed"}'
        return wrk.format(nil, "/api/v1/tasks/task-1")
    end
end

function response(status, headers, body)
    if status >= 400 then
        print("Error " .. status .. ": " .. body)
    end
end

function done(summary, latency, requests)
    io.write("------------------------------\n")
    io.write("Requests/sec: " .. summary.requests / summary.duration * 1e6 .. "\n")
    io.write("Transfer/sec: " .. summary.bytes / summary.duration * 1e6 .. "\n")
    io.write("Avg Latency:  " .. latency.mean / 1000 .. "ms\n")
    io.write("Max Latency:  " .. latency.max / 1000 .. "ms\n")
    io.write("Stdev:        " .. latency.stdev / 1000 .. "ms\n")
    io.write("------------------------------\n")
end
EOF

# Generate comparison report
generate_report() {
    echo "Generating performance report..."
    
    cat > "$RESULTS_DIR/report_${TIMESTAMP}.md" <<EOF
# Performance Benchmark Report
Generated: $(date)

## Configuration
- Concurrent connections: $CONCURRENCY
- Total requests: $REQUESTS
- Test duration: ${DURATION}s

## Results Summary

| Language | REST RPS | REST P95 Latency | gRPC RPS | gRPC P95 Latency | Winner |
|----------|----------|------------------|----------|------------------|--------|
EOF
    
    # Parse and add results for each language
    for lang in "${LANGUAGES[@]}"; do
        if [ -f "$RESULTS_DIR/${lang}_rest_wrk_${TIMESTAMP}.txt" ]; then
            # Extract metrics from wrk output
            REST_RPS=$(grep "Requests/sec" "$RESULTS_DIR/${lang}_rest_wrk_${TIMESTAMP}.txt" | awk '{print $2}')
            REST_P95=$(grep "95%" "$RESULTS_DIR/${lang}_rest_wrk_${TIMESTAMP}.txt" | awk '{print $2}')
        else
            REST_RPS="N/A"
            REST_P95="N/A"
        fi
        
        if [ -f "$RESULTS_DIR/${lang}_grpc_${TIMESTAMP}.json" ]; then
            # Extract metrics from ghz output
            GRPC_RPS=$(jq '.rps' "$RESULTS_DIR/${lang}_grpc_${TIMESTAMP}.json" 2>/dev/null || echo "N/A")
            GRPC_P95=$(jq '.latency.p95' "$RESULTS_DIR/${lang}_grpc_${TIMESTAMP}.json" 2>/dev/null || echo "N/A")
        else
            GRPC_RPS="N/A"
            GRPC_P95="N/A"
        fi
        
        # Determine winner
        if [ "$REST_RPS" != "N/A" ] && [ "$GRPC_RPS" != "N/A" ]; then
            if (( $(echo "$GRPC_RPS > $REST_RPS" | bc -l) )); then
                WINNER="gRPC"
            else
                WINNER="REST"
            fi
        else
            WINNER="-"
        fi
        
        echo "| $lang | $REST_RPS | $REST_P95 | $GRPC_RPS | $GRPC_P95 | $WINNER |" >> "$RESULTS_DIR/report_${TIMESTAMP}.md"
    done
    
    echo "" >> "$RESULTS_DIR/report_${TIMESTAMP}.md"
    echo "## Analysis" >> "$RESULTS_DIR/report_${TIMESTAMP}.md"
    echo "- gRPC generally shows better performance for structured data" >> "$RESULTS_DIR/report_${TIMESTAMP}.md"
    echo "- REST has lower complexity and better debugging tools" >> "$RESULTS_DIR/report_${TIMESTAMP}.md"
    echo "- Language-specific optimizations can significantly impact results" >> "$RESULTS_DIR/report_${TIMESTAMP}.md"
}

# Check dependencies
check_dependencies() {
    echo "Checking dependencies..."
    
    if ! command -v wrk &> /dev/null; then
        echo "Installing wrk..."
        if [[ "$OSTYPE" == "darwin"* ]]; then
            brew install wrk
        else
            echo "Please install wrk: https://github.com/wg/wrk"
        fi
    fi
    
    if ! command -v ghz &> /dev/null; then
        echo "Installing ghz..."
        if [[ "$OSTYPE" == "darwin"* ]]; then
            brew install ghz
        else
            echo "Please install ghz: https://ghz.sh"
        fi
    fi
}

# Main execution
main() {
    check_dependencies
    
    echo "Starting benchmarks..."
    echo
    
    for lang in "${LANGUAGES[@]}"; do
        echo "Testing $lang..."
        
        # Start the server
        cd "code/$lang/rest/server"
        # Server should be started separately
        # ./start.sh &
        # SERVER_PID=$!
        
        # Wait for server to be ready
        sleep 5
        
        # Run benchmarks
        benchmark_rest "$lang"
        benchmark_grpc "$lang"
        
        # Stop server
        # kill $SERVER_PID 2>/dev/null || true
        
        cd - > /dev/null
        echo
    done
    
    generate_report
    
    echo "Benchmark complete! Results saved to $RESULTS_DIR"
    echo "Report: $RESULTS_DIR/report_${TIMESTAMP}.md"
}

# Run if not sourced
if [ "${BASH_SOURCE[0]}" == "${0}" ]; then
    main "$@"
fi