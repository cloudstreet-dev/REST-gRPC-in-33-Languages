#!/bin/bash

# Universal Test Suite for REST APIs
# Tests any language implementation for API compliance

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Configuration
BASE_URL="${BASE_URL:-http://localhost:8080}"
API_PATH="/api/v1/tasks"
TEST_RESULTS_DIR="test-results"
LANGUAGE="${1:-unknown}"

# Create results directory
mkdir -p "$TEST_RESULTS_DIR"

# Test counter
TESTS_PASSED=0
TESTS_FAILED=0

# Helper functions
log_test() {
    echo -e "${YELLOW}Testing:${NC} $1"
}

log_pass() {
    echo -e "${GREEN}✓${NC} $1"
    ((TESTS_PASSED++))
}

log_fail() {
    echo -e "${RED}✗${NC} $1"
    echo "  Expected: $2"
    echo "  Got: $3"
    ((TESTS_FAILED++))
}

# Wait for server to be ready
wait_for_server() {
    echo "Waiting for server at $BASE_URL..."
    for i in {1..30}; do
        if curl -s "${BASE_URL}/health" > /dev/null; then
            echo "Server is ready!"
            return 0
        fi
        sleep 1
    done
    echo "Server failed to start!"
    exit 1
}

# Test health endpoint
test_health() {
    log_test "Health endpoint"
    
    RESPONSE=$(curl -s "${BASE_URL}/health")
    STATUS=$(echo "$RESPONSE" | jq -r '.status' 2>/dev/null || echo "")
    
    if [ "$STATUS" == "healthy" ]; then
        log_pass "Health endpoint returns healthy status"
    else
        log_fail "Health endpoint" "status: healthy" "$RESPONSE"
    fi
}

# Test create task
test_create_task() {
    log_test "Create task"
    
    RESPONSE=$(curl -s -X POST "${BASE_URL}${API_PATH}" \
        -H "Content-Type: application/json" \
        -d '{
            "title": "Test Task",
            "description": "Test Description",
            "priority": "high",
            "tags": ["test", "api"]
        }')
    
    TASK_ID=$(echo "$RESPONSE" | jq -r '.id' 2>/dev/null || echo "")
    TITLE=$(echo "$RESPONSE" | jq -r '.title' 2>/dev/null || echo "")
    
    if [ -n "$TASK_ID" ] && [ "$TITLE" == "Test Task" ]; then
        log_pass "Task created successfully"
        echo "$TASK_ID" > "${TEST_RESULTS_DIR}/task_id.txt"
    else
        log_fail "Create task" "task with id and title" "$RESPONSE"
    fi
}

# Test list tasks
test_list_tasks() {
    log_test "List tasks"
    
    RESPONSE=$(curl -s "${BASE_URL}${API_PATH}")
    TASK_COUNT=$(echo "$RESPONSE" | jq '.tasks | length' 2>/dev/null || echo "0")
    
    if [ "$TASK_COUNT" -gt 0 ]; then
        log_pass "List tasks returns $TASK_COUNT tasks"
    else
        log_fail "List tasks" "array of tasks" "$RESPONSE"
    fi
}

# Test get single task
test_get_task() {
    log_test "Get single task"
    
    if [ -f "${TEST_RESULTS_DIR}/task_id.txt" ]; then
        TASK_ID=$(cat "${TEST_RESULTS_DIR}/task_id.txt")
        RESPONSE=$(curl -s "${BASE_URL}${API_PATH}/${TASK_ID}")
        
        FOUND_ID=$(echo "$RESPONSE" | jq -r '.id' 2>/dev/null || echo "")
        
        if [ "$FOUND_ID" == "$TASK_ID" ]; then
            log_pass "Retrieved task by ID"
        else
            log_fail "Get task" "task with id: $TASK_ID" "$RESPONSE"
        fi
    else
        echo "  Skipping: No task ID available"
    fi
}

# Test update task
test_update_task() {
    log_test "Update task"
    
    if [ -f "${TEST_RESULTS_DIR}/task_id.txt" ]; then
        TASK_ID=$(cat "${TEST_RESULTS_DIR}/task_id.txt")
        RESPONSE=$(curl -s -X PUT "${BASE_URL}${API_PATH}/${TASK_ID}" \
            -H "Content-Type: application/json" \
            -d '{
                "title": "Updated Task",
                "status": "in_progress"
            }')
        
        TITLE=$(echo "$RESPONSE" | jq -r '.title' 2>/dev/null || echo "")
        STATUS=$(echo "$RESPONSE" | jq -r '.status' 2>/dev/null || echo "")
        
        if [ "$TITLE" == "Updated Task" ] && [ "$STATUS" == "in_progress" ]; then
            log_pass "Task updated successfully"
        else
            log_fail "Update task" "updated title and status" "$RESPONSE"
        fi
    else
        echo "  Skipping: No task ID available"
    fi
}

# Test update task status
test_update_status() {
    log_test "Update task status"
    
    if [ -f "${TEST_RESULTS_DIR}/task_id.txt" ]; then
        TASK_ID=$(cat "${TEST_RESULTS_DIR}/task_id.txt")
        RESPONSE=$(curl -s -X PATCH "${BASE_URL}${API_PATH}/${TASK_ID}/status" \
            -H "Content-Type: application/json" \
            -d '{"status": "completed"}')
        
        STATUS=$(echo "$RESPONSE" | jq -r '.status' 2>/dev/null || echo "")
        
        if [ "$STATUS" == "completed" ]; then
            log_pass "Task status updated"
        else
            log_fail "Update status" "status: completed" "$RESPONSE"
        fi
    else
        echo "  Skipping: No task ID available"
    fi
}

# Test delete task
test_delete_task() {
    log_test "Delete task"
    
    if [ -f "${TEST_RESULTS_DIR}/task_id.txt" ]; then
        TASK_ID=$(cat "${TEST_RESULTS_DIR}/task_id.txt")
        RESPONSE_CODE=$(curl -s -o /dev/null -w "%{http_code}" \
            -X DELETE "${BASE_URL}${API_PATH}/${TASK_ID}")
        
        if [ "$RESPONSE_CODE" == "204" ] || [ "$RESPONSE_CODE" == "200" ]; then
            log_pass "Task deleted successfully"
        else
            log_fail "Delete task" "204 or 200" "$RESPONSE_CODE"
        fi
    else
        echo "  Skipping: No task ID available"
    fi
}

# Test query parameters
test_query_params() {
    log_test "Query parameters"
    
    # Create a task with specific status
    curl -s -X POST "${BASE_URL}${API_PATH}" \
        -H "Content-Type: application/json" \
        -d '{"title": "Pending Task", "status": "pending"}' > /dev/null
    
    RESPONSE=$(curl -s "${BASE_URL}${API_PATH}?status=pending")
    TASKS=$(echo "$RESPONSE" | jq '.tasks' 2>/dev/null || echo "null")
    
    if [ "$TASKS" != "null" ]; then
        log_pass "Query parameters work"
    else
        log_fail "Query parameters" "filtered tasks" "$RESPONSE"
    fi
}

# Test error handling
test_error_handling() {
    log_test "Error handling"
    
    # Test 404
    RESPONSE_CODE=$(curl -s -o /dev/null -w "%{http_code}" \
        "${BASE_URL}${API_PATH}/nonexistent-id")
    
    if [ "$RESPONSE_CODE" == "404" ]; then
        log_pass "404 error handling works"
    else
        log_fail "404 handling" "404" "$RESPONSE_CODE"
    fi
    
    # Test 400
    RESPONSE=$(curl -s -o /dev/null -w "%{http_code}" -X POST "${BASE_URL}${API_PATH}" \
        -H "Content-Type: application/json" \
        -d '{}')
    
    if [ "$RESPONSE" == "400" ]; then
        log_pass "400 error handling works"
    else
        log_fail "400 handling" "400" "$RESPONSE"
    fi
}

# Main test execution
main() {
    echo "======================================"
    echo "Universal REST API Test Suite"
    echo "Testing: $LANGUAGE"
    echo "URL: ${BASE_URL}${API_PATH}"
    echo "======================================"
    echo
    
    wait_for_server
    
    test_health
    test_create_task
    test_list_tasks
    test_get_task
    test_update_task
    test_update_status
    test_delete_task
    test_query_params
    test_error_handling
    
    echo
    echo "======================================"
    echo "Test Results for $LANGUAGE"
    echo "======================================"
    echo -e "${GREEN}Passed:${NC} $TESTS_PASSED"
    echo -e "${RED}Failed:${NC} $TESTS_FAILED"
    
    # Save results
    cat > "${TEST_RESULTS_DIR}/${LANGUAGE}-results.json" <<EOF
{
    "language": "$LANGUAGE",
    "passed": $TESTS_PASSED,
    "failed": $TESTS_FAILED,
    "total": $((TESTS_PASSED + TESTS_FAILED)),
    "timestamp": "$(date -u +"%Y-%m-%dT%H:%M:%SZ")"
}
EOF
    
    if [ "$TESTS_FAILED" -gt 0 ]; then
        echo -e "${RED}Some tests failed!${NC}"
        exit 1
    else
        echo -e "${GREEN}All tests passed!${NC}"
    fi
}

# Check dependencies
if ! command -v curl &> /dev/null; then
    echo "curl is required but not installed"
    exit 1
fi

if ! command -v jq &> /dev/null; then
    echo "jq is required but not installed"
    exit 1
fi

main "$@"