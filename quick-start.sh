#!/bin/bash

# Quick Start Script for REST & gRPC in 33 Languages
# Interactive launcher for exploring different language implementations

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
MAGENTA='\033[0;35m'
CYAN='\033[0;36m'
NC='\033[0m'

# Configuration
BASE_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CODE_DIR="${BASE_DIR}/code"

# ASCII Art Banner
show_banner() {
    echo -e "${CYAN}"
    cat << "EOF"
    ____  _____ ____ _____    ___        ____  ____   ____ 
   |  _ \| ____/ ___|_   _|  ( _ )      / ___|  _ \ / ___|
   | |_) |  _| \___ \ | |    / _ \/\   | |  _| |_) | |    
   |  _ <| |___ ___) || |   | (_>  <   | |_| |  _ <| |___ 
   |_| \_\_____|____/ |_|    \___/\/    \____|_| \_\\____|
                                                           
            in 33 Programming Languages
EOF
    echo -e "${NC}"
}

# Show available languages
show_languages() {
    echo -e "${YELLOW}Available Languages:${NC}"
    echo
    echo -e "${GREEN}Full Support (REST + gRPC):${NC}"
    echo "  1. JavaScript    2. TypeScript    3. Go           4. Python"
    echo "  5. Java          6. Rust          7. Kotlin       8. Swift"
    echo "  9. C#           10. F#           11. C++         12. Scala"
    echo " 13. Ruby         14. PHP          15. Dart        16. Perl"
    echo " 17. Clojure      18. Erlang       19. Elixir      20. Haskell"
    echo
    echo -e "${BLUE}REST Only:${NC}"
    echo " 21. Crystal      22. Lua          23. C           24. R"
    echo " 25. MATLAB       26. Fortran      27. COBOL       28. Ada"
    echo " 29. Zig          30. OCaml        31. Nim         32. Julia"
    echo " 33. Gleam        34. V            35. Io"
}

# Get language name from number
get_language() {
    case $1 in
        1) echo "javascript";;
        2) echo "typescript";;
        3) echo "go";;
        4) echo "python";;
        5) echo "java";;
        6) echo "rust";;
        7) echo "kotlin";;
        8) echo "swift";;
        9) echo "csharp";;
        10) echo "fsharp";;
        11) echo "cpp";;
        12) echo "scala";;
        13) echo "ruby";;
        14) echo "php";;
        15) echo "dart";;
        16) echo "perl";;
        17) echo "clojure";;
        18) echo "erlang";;
        19) echo "elixir";;
        20) echo "haskell";;
        21) echo "crystal";;
        22) echo "lua";;
        23) echo "c";;
        24) echo "r";;
        25) echo "matlab";;
        26) echo "fortran";;
        27) echo "cobol";;
        28) echo "ada";;
        29) echo "zig";;
        30) echo "ocaml";;
        31) echo "nim";;
        32) echo "julia";;
        33) echo "gleam";;
        34) echo "v";;
        35) echo "io";;
        *) echo "";;
    esac
}

# Check if language has gRPC support
has_grpc() {
    case $1 in
        javascript|typescript|go|python|java|rust|kotlin|swift|csharp|fsharp|cpp|scala|ruby|php|dart|perl|clojure|erlang|elixir|haskell)
            return 0;;
        *)
            return 1;;
    esac
}

# Start a service
start_service() {
    local LANG=$1
    local TYPE=$2
    local DIR="${CODE_DIR}/${LANG}/${TYPE}/server"
    
    if [ ! -d "$DIR" ]; then
        echo -e "${RED}Directory not found: $DIR${NC}"
        return 1
    fi
    
    echo -e "${YELLOW}Starting ${LANG} ${TYPE} server...${NC}"
    
    # Check for run script
    if [ -f "${DIR}/run.sh" ]; then
        cd "$DIR"
        bash run.sh &
        local PID=$!
        echo -e "${GREEN}Started ${LANG} ${TYPE} server (PID: ${PID})${NC}"
        echo "$PID" > "/tmp/${LANG}_${TYPE}.pid"
    elif [ -f "${DIR}/Dockerfile" ]; then
        echo "Starting with Docker..."
        docker build -t "${LANG}-${TYPE}" "$DIR" > /dev/null 2>&1
        docker run -d -p 8080:8080 --name "${LANG}-${TYPE}" "${LANG}-${TYPE}"
        echo -e "${GREEN}Started ${LANG} ${TYPE} server in Docker${NC}"
    else
        echo -e "${RED}No run.sh or Dockerfile found${NC}"
        return 1
    fi
    
    # Wait for service to be ready
    echo "Waiting for service to be ready..."
    local PORT=8080
    if [ "$TYPE" == "grpc" ]; then
        PORT=50051
    fi
    
    for i in {1..30}; do
        if nc -z localhost $PORT 2>/dev/null; then
            echo -e "${GREEN}Service is ready on port ${PORT}!${NC}"
            return 0
        fi
        sleep 1
    done
    
    echo -e "${YELLOW}Service may not be fully ready yet${NC}"
}

# Stop a service
stop_service() {
    local LANG=$1
    local TYPE=$2
    
    # Check for PID file
    if [ -f "/tmp/${LANG}_${TYPE}.pid" ]; then
        local PID=$(cat "/tmp/${LANG}_${TYPE}.pid")
        if kill -0 $PID 2>/dev/null; then
            kill $PID
            rm "/tmp/${LANG}_${TYPE}.pid"
            echo -e "${GREEN}Stopped ${LANG} ${TYPE} server${NC}"
        fi
    fi
    
    # Check for Docker container
    if docker ps -a | grep -q "${LANG}-${TYPE}"; then
        docker stop "${LANG}-${TYPE}" > /dev/null 2>&1
        docker rm "${LANG}-${TYPE}" > /dev/null 2>&1
        echo -e "${GREEN}Stopped ${LANG} ${TYPE} Docker container${NC}"
    fi
}

# Run tests for a service
run_tests() {
    local LANG=$1
    local TYPE=$2
    
    echo -e "${YELLOW}Running tests for ${LANG} ${TYPE}...${NC}"
    
    if [ "$TYPE" == "rest" ]; then
        bash "${BASE_DIR}/test/universal-test-suite.sh" "$LANG"
    else
        echo -e "${BLUE}gRPC testing requires ghz tool${NC}"
        if command -v ghz &> /dev/null; then
            ghz --insecure \
                --proto "${CODE_DIR}/shared/protos/tasks.proto" \
                --call tasks.v1.TaskService.ListTasks \
                -d '{"page_size": 20}' \
                -c 10 -n 100 \
                localhost:50051
        else
            echo "Install ghz: https://ghz.sh"
        fi
    fi
}

# Interactive menu
interactive_menu() {
    while true; do
        echo
        echo -e "${CYAN}╔══════════════════════════════════════╗${NC}"
        echo -e "${CYAN}║     REST & gRPC Quick Start Menu     ║${NC}"
        echo -e "${CYAN}╚══════════════════════════════════════╝${NC}"
        echo
        echo "1) Start a service"
        echo "2) Stop a service"
        echo "3) Run tests"
        echo "4) Run benchmarks"
        echo "5) View documentation"
        echo "6) Start all services (Docker)"
        echo "7) Interactive comparison"
        echo "8) Learning path"
        echo "0) Exit"
        echo
        read -p "Select an option: " choice
        
        case $choice in
            1)
                show_languages
                echo
                read -p "Select language (1-35): " lang_num
                LANG=$(get_language $lang_num)
                
                if [ -z "$LANG" ]; then
                    echo -e "${RED}Invalid selection${NC}"
                    continue
                fi
                
                if has_grpc "$LANG"; then
                    echo "1) REST API"
                    echo "2) gRPC"
                    read -p "Select type: " type_choice
                    case $type_choice in
                        1) TYPE="rest";;
                        2) TYPE="grpc";;
                        *) echo -e "${RED}Invalid selection${NC}"; continue;;
                    esac
                else
                    TYPE="rest"
                fi
                
                start_service "$LANG" "$TYPE"
                ;;
            2)
                read -p "Enter language name: " LANG
                read -p "Enter type (rest/grpc): " TYPE
                stop_service "$LANG" "$TYPE"
                ;;
            3)
                read -p "Enter language name: " LANG
                read -p "Enter type (rest/grpc): " TYPE
                run_tests "$LANG" "$TYPE"
                ;;
            4)
                echo "Starting benchmark suite..."
                bash "${BASE_DIR}/benchmark/benchmark.sh"
                ;;
            5)
                echo "Opening documentation..."
                if command -v open &> /dev/null; then
                    open "${BASE_DIR}/README.md"
                elif command -v xdg-open &> /dev/null; then
                    xdg-open "${BASE_DIR}/README.md"
                else
                    echo "Documentation at: ${BASE_DIR}/README.md"
                fi
                ;;
            6)
                echo "Starting all services with Docker Compose..."
                docker-compose up -d
                echo -e "${GREEN}All services started!${NC}"
                echo "View logs: docker-compose logs -f"
                ;;
            7)
                interactive_comparison
                ;;
            8)
                show_learning_path
                ;;
            0)
                echo "Goodbye!"
                exit 0
                ;;
            *)
                echo -e "${RED}Invalid option${NC}"
                ;;
        esac
    done
}

# Interactive comparison
interactive_comparison() {
    echo -e "${CYAN}Interactive Language Comparison${NC}"
    echo
    echo "Select two languages to compare:"
    show_languages
    echo
    read -p "First language (1-35): " lang1_num
    read -p "Second language (1-35): " lang2_num
    
    LANG1=$(get_language $lang1_num)
    LANG2=$(get_language $lang2_num)
    
    if [ -z "$LANG1" ] || [ -z "$LANG2" ]; then
        echo -e "${RED}Invalid selection${NC}"
        return
    fi
    
    echo
    echo -e "${YELLOW}Comparing ${LANG1} vs ${LANG2}${NC}"
    echo
    
    # Start both services
    start_service "$LANG1" "rest"
    start_service "$LANG2" "rest"
    
    # Wait a moment
    sleep 3
    
    # Run benchmarks
    echo -e "${CYAN}Running performance comparison...${NC}"
    
    # Simple benchmark using curl
    echo -e "\n${LANG1} Performance:"
    time for i in {1..100}; do
        curl -s "http://localhost:8080/api/v1/tasks" > /dev/null
    done
    
    # Switch ports if needed
    echo -e "\n${LANG2} Performance:"
    time for i in {1..100}; do
        curl -s "http://localhost:8081/api/v1/tasks" > /dev/null
    done
    
    # Stop services
    stop_service "$LANG1" "rest"
    stop_service "$LANG2" "rest"
}

# Show learning path
show_learning_path() {
    echo -e "${CYAN}╔══════════════════════════════════════╗${NC}"
    echo -e "${CYAN}║         Learning Path Guide          ║${NC}"
    echo -e "${CYAN}╚══════════════════════════════════════╝${NC}"
    echo
    echo -e "${GREEN}Beginner Path:${NC}"
    echo "1. Start with JavaScript (Chapter 1) - Simple and familiar"
    echo "2. Move to Python (Chapter 4) - Clean syntax, great for APIs"
    echo "3. Try Go (Chapter 3) - Modern, simple, great performance"
    echo
    echo -e "${YELLOW}Intermediate Path:${NC}"
    echo "1. Explore TypeScript (Chapter 2) - Type safety"
    echo "2. Learn Rust (Chapter 6) - Memory safety without GC"
    echo "3. Try Kotlin (Chapter 7) - Modern JVM language"
    echo
    echo -e "${MAGENTA}Advanced Path:${NC}"
    echo "1. Master Haskell (Chapter 35) - Pure functional"
    echo "2. Explore Erlang (Chapter 27) - Actor model"
    echo "3. Learn Clojure (Chapter 25) - Lisp on JVM"
    echo
    echo -e "${BLUE}Performance Focus:${NC}"
    echo "1. C++ (Chapter 11) - Maximum control"
    echo "2. Rust (Chapter 6) - Safe and fast"
    echo "3. Go (Chapter 3) - Concurrent and efficient"
    echo
    echo -e "${CYAN}Functional Programming:${NC}"
    echo "1. Haskell (Chapter 35) - Pure FP"
    echo "2. F# (Chapter 10) - FP on .NET"
    echo "3. Elixir (Chapter 34) - FP with actor model"
    echo
    read -p "Press Enter to continue..."
}

# Check dependencies
check_dependencies() {
    echo -e "${YELLOW}Checking dependencies...${NC}"
    
    local MISSING=()
    
    # Check for required tools
    command -v docker &> /dev/null || MISSING+=("docker")
    command -v curl &> /dev/null || MISSING+=("curl")
    command -v nc &> /dev/null || MISSING+=("netcat")
    
    if [ ${#MISSING[@]} -gt 0 ]; then
        echo -e "${RED}Missing dependencies:${NC}"
        for dep in "${MISSING[@]}"; do
            echo "  - $dep"
        done
        echo
        echo "Please install missing dependencies to continue"
        exit 1
    fi
    
    echo -e "${GREEN}All dependencies satisfied!${NC}"
}

# Main execution
main() {
    show_banner
    check_dependencies
    
    # Parse command line arguments
    if [ $# -eq 0 ]; then
        interactive_menu
    else
        case $1 in
            start)
                if [ $# -ge 3 ]; then
                    start_service "$2" "$3"
                else
                    echo "Usage: $0 start <language> <rest|grpc>"
                fi
                ;;
            stop)
                if [ $# -ge 3 ]; then
                    stop_service "$2" "$3"
                else
                    echo "Usage: $0 stop <language> <rest|grpc>"
                fi
                ;;
            test)
                if [ $# -ge 3 ]; then
                    run_tests "$2" "$3"
                else
                    echo "Usage: $0 test <language> <rest|grpc>"
                fi
                ;;
            benchmark)
                bash "${BASE_DIR}/benchmark/benchmark.sh"
                ;;
            list)
                show_languages
                ;;
            help|--help|-h)
                echo "REST & gRPC in 33 Languages - Quick Start"
                echo
                echo "Usage: $0 [command] [options]"
                echo
                echo "Commands:"
                echo "  start <lang> <type>  Start a service"
                echo "  stop <lang> <type>   Stop a service"
                echo "  test <lang> <type>   Run tests"
                echo "  benchmark            Run performance benchmarks"
                echo "  list                 List all languages"
                echo "  help                 Show this help"
                echo
                echo "Interactive mode: Run without arguments"
                ;;
            *)
                echo -e "${RED}Unknown command: $1${NC}"
                echo "Run '$0 help' for usage"
                exit 1
                ;;
        esac
    fi
}

# Cleanup on exit
cleanup() {
    echo -e "\n${YELLOW}Cleaning up...${NC}"
    # Stop any running services
    for pidfile in /tmp/*_*.pid; do
        if [ -f "$pidfile" ]; then
            PID=$(cat "$pidfile")
            kill $PID 2>/dev/null || true
            rm "$pidfile"
        fi
    done
}

trap cleanup EXIT

# Run main
main "$@"