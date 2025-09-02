#!/bin/bash

# Development Environment Setup for REST & gRPC in 33 Languages
# Installs all required tools and runtimes

set -e

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
NC='\033[0m'

# OS Detection
OS=""
if [[ "$OSTYPE" == "linux-gnu"* ]]; then
    OS="linux"
elif [[ "$OSTYPE" == "darwin"* ]]; then
    OS="macos"
elif [[ "$OSTYPE" == "cygwin" || "$OSTYPE" == "msys" ]]; then
    OS="windows"
else
    echo -e "${RED}Unsupported OS: $OSTYPE${NC}"
    exit 1
fi

echo -e "${CYAN}"
cat << "EOF"
 ____             ____            ____        _               
|  _ \  _____   __/ ___|  ___     | __ )  ___ | |_             
| | | |/ _ \ \ / /\___ \ / _ \    |  _ \ / _ \| __|            
| |_| |  __/\ V /  ___) |  __/    | |_) | (_) | |_             
|____/ \___| \_/  |____/ \___|    |____/ \___/ \__|            
                                                               
    Environment Setup for 33 Languages
EOF
echo -e "${NC}"

echo -e "${YELLOW}Setting up development environment for $OS...${NC}"
echo

# Package managers
install_package_managers() {
    echo -e "${BLUE}Installing package managers...${NC}"
    
    case $OS in
        "linux")
            # Update package lists
            sudo apt-get update
            
            # Install Homebrew for Linux
            if ! command -v brew &> /dev/null; then
                echo "Installing Homebrew..."
                /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
                echo 'eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"' >> ~/.bashrc
                eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"
            fi
            
            # Install snap
            if ! command -v snap &> /dev/null; then
                sudo apt-get install -y snapd
            fi
            ;;
        "macos")
            # Install Homebrew
            if ! command -v brew &> /dev/null; then
                echo "Installing Homebrew..."
                /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
            fi
            ;;
        "windows")
            # Install Chocolatey
            if ! command -v choco &> /dev/null; then
                echo "Installing Chocolatey..."
                powershell -Command "Set-ExecutionPolicy Bypass -Scope Process -Force; [System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072; iex ((New-Object System.Net.WebClient).DownloadString('https://chocolatey.org/install.ps1'))"
            fi
            ;;
    esac
}

# Core development tools
install_core_tools() {
    echo -e "${BLUE}Installing core development tools...${NC}"
    
    case $OS in
        "linux"|"macos")
            brew install curl wget git jq make cmake build-essential protobuf
            ;;
        "windows")
            choco install curl wget git jq make cmake visualstudio2022buildtools protoc
            ;;
    esac
}

# JavaScript/TypeScript ecosystem
install_js_ecosystem() {
    echo -e "${BLUE}Installing JavaScript/TypeScript ecosystem...${NC}"
    
    # Install Node.js via Node Version Manager
    if ! command -v node &> /dev/null; then
        case $OS in
            "linux"|"macos")
                # Install nvm
                curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.0/install.sh | bash
                export NVM_DIR="$HOME/.nvm"
                [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"
                
                # Install latest LTS Node
                nvm install --lts
                nvm use --lts
                ;;
            "windows")
                choco install nodejs npm
                ;;
        esac
    fi
    
    # Install TypeScript globally
    npm install -g typescript ts-node
    
    # Install common tools
    npm install -g @grpc/grpc-js grpc-tools prettier eslint
}

# Go ecosystem
install_go() {
    echo -e "${BLUE}Installing Go...${NC}"
    
    case $OS in
        "linux"|"macos")
            brew install go
            ;;
        "windows")
            choco install golang
            ;;
    esac
    
    # Install Go tools
    go install google.golang.org/protobuf/cmd/protoc-gen-go@latest
    go install google.golang.org/grpc/cmd/protoc-gen-go-grpc@latest
    go install github.com/golangci/golangci-lint/cmd/golangci-lint@latest
}

# Python ecosystem
install_python() {
    echo -e "${BLUE}Installing Python ecosystem...${NC}"
    
    case $OS in
        "linux"|"macos")
            # Install pyenv for Python version management
            if ! command -v pyenv &> /dev/null; then
                curl https://pyenv.run | bash
                echo 'export PATH="$HOME/.pyenv/bin:$PATH"' >> ~/.bashrc
                echo 'eval "$(pyenv init -)"' >> ~/.bashrc
                echo 'eval "$(pyenv virtualenv-init -)"' >> ~/.bashrc
                export PATH="$HOME/.pyenv/bin:$PATH"
                eval "$(pyenv init -)"
                eval "$(pyenv virtualenv-init -)"
            fi
            
            # Install Python 3.11
            pyenv install 3.11.0
            pyenv global 3.11.0
            ;;
        "windows")
            choco install python
            ;;
    esac
    
    # Install Python packages
    pip install --upgrade pip
    pip install grpcio grpcio-tools fastapi uvicorn pytest black pylint mypy
}

# Java ecosystem
install_java() {
    echo -e "${BLUE}Installing Java ecosystem...${NC}"
    
    case $OS in
        "linux"|"macos")
            # Install SDKMAN for Java version management
            if ! command -v sdk &> /dev/null; then
                curl -s "https://get.sdkman.io" | bash
                source "$HOME/.sdkman/bin/sdkman-init.sh"
            fi
            
            # Install Java 17
            sdk install java 17.0.8-tem
            sdk use java 17.0.8-tem
            
            # Install Maven and Gradle
            sdk install maven
            sdk install gradle
            ;;
        "windows")
            choco install openjdk17 maven gradle
            ;;
    esac
}

# Rust ecosystem
install_rust() {
    echo -e "${BLUE}Installing Rust...${NC}"
    
    if ! command -v rustc &> /dev/null; then
        curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
        source $HOME/.cargo/env
    fi
    
    # Install Rust tools
    cargo install cargo-edit cargo-watch
    rustup component add clippy rustfmt
}

# .NET ecosystem
install_dotnet() {
    echo -e "${BLUE}Installing .NET ecosystem...${NC}"
    
    case $OS in
        "linux")
            wget https://packages.microsoft.com/config/ubuntu/22.04/packages-microsoft-prod.deb -O packages-microsoft-prod.deb
            sudo dpkg -i packages-microsoft-prod.deb
            rm packages-microsoft-prod.deb
            sudo apt-get update
            sudo apt-get install -y dotnet-sdk-7.0
            ;;
        "macos")
            brew install --cask dotnet
            ;;
        "windows")
            choco install dotnet-sdk
            ;;
    esac
}

# Ruby ecosystem
install_ruby() {
    echo -e "${BLUE}Installing Ruby...${NC}"
    
    case $OS in
        "linux"|"macos")
            # Install rbenv for Ruby version management
            if ! command -v rbenv &> /dev/null; then
                case $OS in
                    "linux")
                        curl -fsSL https://github.com/rbenv/rbenv-installer/raw/HEAD/bin/rbenv-installer | bash
                        ;;
                    "macos")
                        brew install rbenv
                        ;;
                esac
                echo 'eval "$(rbenv init -)"' >> ~/.bashrc
                eval "$(rbenv init -)"
            fi
            
            # Install Ruby 3.2
            rbenv install 3.2.0
            rbenv global 3.2.0
            ;;
        "windows")
            choco install ruby
            ;;
    esac
    
    # Install common gems
    gem install bundler sinatra grpc
}

# Elixir/Erlang
install_elixir() {
    echo -e "${BLUE}Installing Elixir/Erlang...${NC}"
    
    case $OS in
        "linux"|"macos")
            # Install asdf for version management
            if ! command -v asdf &> /dev/null; then
                git clone https://github.com/asdf-vm/asdf.git ~/.asdf --branch v0.13.1
                echo '. $HOME/.asdf/asdf.sh' >> ~/.bashrc
                echo '. $HOME/.asdf/completions/asdf.bash' >> ~/.bashrc
                . $HOME/.asdf/asdf.sh
            fi
            
            # Add plugins
            asdf plugin add erlang
            asdf plugin add elixir
            
            # Install Erlang and Elixir
            asdf install erlang 26.1
            asdf install elixir 1.15.6-otp-26
            asdf global erlang 26.1
            asdf global elixir 1.15.6-otp-26
            ;;
        "windows")
            choco install erlang elixir
            ;;
    esac
    
    # Install Hex and Phoenix
    mix local.hex --force
    mix local.rebar --force
    mix archive.install hex phx_new --force
}

# Haskell
install_haskell() {
    echo -e "${BLUE}Installing Haskell...${NC}"
    
    case $OS in
        "linux"|"macos")
            curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
            ;;
        "windows")
            choco install ghc cabal
            ;;
    esac
}

# Specialized tools
install_specialized_tools() {
    echo -e "${BLUE}Installing specialized language tools...${NC}"
    
    case $OS in
        "linux"|"macos")
            # Swift (Linux only has limited support)
            if [[ $OS == "macos" ]]; then
                # Swift comes with Xcode on macOS
                xcode-select --install 2>/dev/null || true
            fi
            
            # Kotlin
            brew install kotlin
            
            # Scala
            brew install scala sbt
            
            # Clojure
            brew install clojure/tools/clojure leiningen
            
            # Zig
            brew install zig
            
            # Crystal
            brew install crystal
            
            # Nim
            brew install nim
            
            # Julia
            brew install julia
            
            # V
            brew install vlang
            ;;
        "windows")
            choco install kotlin scala sbt zig crystal nim julia
            ;;
    esac
}

# Testing and benchmarking tools
install_testing_tools() {
    echo -e "${BLUE}Installing testing and benchmarking tools...${NC}"
    
    case $OS in
        "linux"|"macos")
            # wrk for HTTP benchmarking
            brew install wrk
            
            # ghz for gRPC benchmarking
            brew install ghz
            
            # grpcurl for gRPC testing
            brew install grpcurl
            
            # Apache Bench
            case $OS in
                "linux")
                    sudo apt-get install -y apache2-utils
                    ;;
                "macos")
                    # ab comes with macOS
                    ;;
            esac
            ;;
        "windows")
            # Install via GitHub releases
            echo "Please install wrk, ghz, and grpcurl manually from GitHub releases"
            ;;
    esac
}

# Docker
install_docker() {
    echo -e "${BLUE}Installing Docker...${NC}"
    
    case $OS in
        "linux")
            curl -fsSL https://get.docker.com -o get-docker.sh
            sudo sh get-docker.sh
            sudo usermod -aG docker $USER
            
            # Install Docker Compose
            sudo curl -L "https://github.com/docker/compose/releases/latest/download/docker-compose-$(uname -s)-$(uname -m)" -o /usr/local/bin/docker-compose
            sudo chmod +x /usr/local/bin/docker-compose
            ;;
        "macos")
            brew install --cask docker
            ;;
        "windows")
            choco install docker-desktop
            ;;
    esac
}

# Configure environment
configure_environment() {
    echo -e "${BLUE}Configuring environment...${NC}"
    
    # Create necessary directories
    mkdir -p ~/.local/bin
    
    # Add to PATH
    echo 'export PATH="$HOME/.local/bin:$PATH"' >> ~/.bashrc
    
    # Create aliases
    cat >> ~/.bashrc << 'EOF'

# REST & gRPC in 33 Languages aliases
alias rgstart='./quick-start.sh'
alias rgtest='make test'
alias rgbench='make benchmark'
alias rgdocker='make docker-up'
alias rghelp='make help'
EOF
    
    # Make scripts executable
    chmod +x quick-start.sh
    chmod +x docker-build-all.sh
    chmod +x benchmark/benchmark.sh
    chmod +x test/universal-test-suite.sh
    chmod +x setup-dev-environment.sh
}

# Verify installations
verify_installations() {
    echo -e "${YELLOW}Verifying installations...${NC}"
    
    local TOOLS=(
        "node:Node.js" "python:Python" "go:Go" "java:Java" "rustc:Rust"
        "dotnet:.NET" "ruby:Ruby" "elixir:Elixir" "ghc:Haskell"
        "docker:Docker" "kubectl:Kubernetes" "curl:curl" "jq:jq"
        "wrk:wrk" "ghz:ghz" "grpcurl:grpcurl"
    )
    
    local MISSING=()
    
    for tool_info in "${TOOLS[@]}"; do
        IFS=':' read -ra TOOL_PARTS <<< "$tool_info"
        local cmd="${TOOL_PARTS[0]}"
        local name="${TOOL_PARTS[1]}"
        
        if command -v "$cmd" &> /dev/null; then
            echo -e "${GREEN}✓${NC} $name"
        else
            echo -e "${RED}✗${NC} $name"
            MISSING+=("$name")
        fi
    done
    
    if [ ${#MISSING[@]} -gt 0 ]; then
        echo -e "\n${YELLOW}Missing tools (optional):${NC}"
        for tool in "${MISSING[@]}"; do
            echo "  - $tool"
        done
    fi
    
    echo -e "\n${GREEN}Core setup complete!${NC}"
}

# Show next steps
show_next_steps() {
    echo -e "\n${CYAN}Next Steps:${NC}"
    echo "==========="
    echo "1. Restart your terminal or run: source ~/.bashrc"
    echo "2. Test the setup: ./quick-start.sh"
    echo "3. Run a language: make run-javascript"
    echo "4. Run tests: make test-all"
    echo "5. Start exploring: make help"
    echo ""
    echo -e "${GREEN}Happy coding with 33 languages! 🚀${NC}"
}

# Main installation flow
main() {
    echo -e "${YELLOW}This script will install development tools for 33 programming languages.${NC}"
    echo -e "${YELLOW}Some installations require sudo access.${NC}"
    echo ""
    read -p "Continue? (y/N): " -n 1 -r
    echo
    
    if [[ ! $REPLY =~ ^[Yy]$ ]]; then
        echo "Installation cancelled."
        exit 0
    fi
    
    echo -e "\n${CYAN}Starting installation...${NC}\n"
    
    install_package_managers
    install_core_tools
    install_js_ecosystem
    install_go
    install_python
    install_java
    install_rust
    install_dotnet
    install_ruby
    install_elixir
    install_haskell
    install_specialized_tools
    install_testing_tools
    install_docker
    configure_environment
    verify_installations
    show_next_steps
}

# Handle interruption
trap 'echo -e "\n${RED}Installation interrupted${NC}"; exit 1' INT

# Check if running with proper permissions
if [[ $EUID -eq 0 ]]; then
   echo -e "${RED}Don't run this script as root${NC}"
   exit 1
fi

main "$@"