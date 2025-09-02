#!/bin/bash

# Docker Build Script for All Services
# Builds Docker images for all language implementations

set -e

# Colors for output
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
NC='\033[0m'

echo "========================================"
echo "Building Docker Images for All Services"
echo "========================================"

# Track build results
SUCCESSFUL_BUILDS=()
FAILED_BUILDS=()

# Function to build a service
build_service() {
    local LANG=$1
    local TYPE=$2
    local PATH=$3
    
    echo -e "${YELLOW}Building ${LANG} ${TYPE}...${NC}"
    
    if [ -f "${PATH}/Dockerfile" ]; then
        if docker build -t "rest-grpc-${LANG}-${TYPE}:latest" "${PATH}" > /dev/null 2>&1; then
            echo -e "${GREEN}✓ ${LANG} ${TYPE} built successfully${NC}"
            SUCCESSFUL_BUILDS+=("${LANG}-${TYPE}")
        else
            echo -e "${RED}✗ ${LANG} ${TYPE} build failed${NC}"
            FAILED_BUILDS+=("${LANG}-${TYPE}")
        fi
    else
        echo "  Dockerfile not found, creating..."
        create_dockerfile "${LANG}" "${TYPE}" "${PATH}"
        if docker build -t "rest-grpc-${LANG}-${TYPE}:latest" "${PATH}" > /dev/null 2>&1; then
            echo -e "${GREEN}✓ ${LANG} ${TYPE} built successfully${NC}"
            SUCCESSFUL_BUILDS+=("${LANG}-${TYPE}")
        else
            echo -e "${RED}✗ ${LANG} ${TYPE} build failed${NC}"
            FAILED_BUILDS+=("${LANG}-${TYPE}")
        fi
    fi
}

# Function to create generic Dockerfile if missing
create_dockerfile() {
    local LANG=$1
    local TYPE=$2
    local PATH=$3
    
    case $LANG in
        "java")
            cat > "${PATH}/Dockerfile" <<'EOF'
FROM maven:3.8-openjdk-17 AS builder
WORKDIR /app
COPY pom.xml .
RUN mvn dependency:go-offline
COPY src ./src
RUN mvn clean package

FROM openjdk:17-slim
WORKDIR /app
COPY --from=builder /app/target/*.jar app.jar
EXPOSE 8080
CMD ["java", "-jar", "app.jar"]
EOF
            ;;
        "kotlin")
            cat > "${PATH}/Dockerfile" <<'EOF'
FROM gradle:7.6-jdk17 AS builder
WORKDIR /app
COPY build.gradle.kts settings.gradle.kts ./
COPY src ./src
RUN gradle build --no-daemon

FROM openjdk:17-slim
WORKDIR /app
COPY --from=builder /app/build/libs/*.jar app.jar
EXPOSE 8080
CMD ["java", "-jar", "app.jar"]
EOF
            ;;
        "csharp")
            cat > "${PATH}/Dockerfile" <<'EOF'
FROM mcr.microsoft.com/dotnet/sdk:7.0 AS builder
WORKDIR /app
COPY *.csproj .
RUN dotnet restore
COPY . .
RUN dotnet publish -c Release -o out

FROM mcr.microsoft.com/dotnet/aspnet:7.0
WORKDIR /app
COPY --from=builder /app/out .
EXPOSE 8080
CMD ["dotnet", "TaskServer.dll"]
EOF
            ;;
        "ruby")
            cat > "${PATH}/Dockerfile" <<'EOF'
FROM ruby:3.2-slim
WORKDIR /app
RUN apt-get update && apt-get install -y build-essential
COPY Gemfile* ./
RUN bundle install
COPY . .
EXPOSE 8080
CMD ["ruby", "app.rb"]
EOF
            ;;
        "elixir")
            cat > "${PATH}/Dockerfile" <<'EOF'
FROM elixir:1.15-alpine AS builder
RUN apk add --no-cache build-base git
WORKDIR /app
RUN mix local.hex --force && mix local.rebar --force
COPY mix.exs mix.lock ./
RUN mix deps.get && mix deps.compile
COPY . .
RUN mix compile

FROM elixir:1.15-alpine
RUN apk add --no-cache openssl ncurses-libs
WORKDIR /app
COPY --from=builder /app/_build /app/_build
COPY --from=builder /app/deps /app/deps
COPY --from=builder /app/mix.* /app/
COPY --from=builder /app/lib /app/lib
EXPOSE 4000
CMD ["mix", "phx.server"]
EOF
            ;;
        "scala")
            cat > "${PATH}/Dockerfile" <<'EOF'
FROM hseeberger/scala-sbt:17.0.2_1.6.2_3.1.1 AS builder
WORKDIR /app
COPY build.sbt .
COPY project ./project
RUN sbt update
COPY src ./src
RUN sbt assembly

FROM openjdk:17-slim
WORKDIR /app
COPY --from=builder /app/target/scala-*/*.jar app.jar
EXPOSE 8080
CMD ["java", "-jar", "app.jar"]
EOF
            ;;
        "haskell")
            cat > "${PATH}/Dockerfile" <<'EOF'
FROM haskell:9.4 AS builder
WORKDIR /app
COPY *.cabal ./
RUN cabal update && cabal build --dependencies-only
COPY . .
RUN cabal build

FROM debian:bookworm-slim
RUN apt-get update && apt-get install -y libgmp10
WORKDIR /app
COPY --from=builder /app/dist-newstyle/build/*/ghc-*/task-server-*/x/task-server/build/task-server/task-server .
EXPOSE 8080
CMD ["./task-server"]
EOF
            ;;
        "clojure")
            cat > "${PATH}/Dockerfile" <<'EOF'
FROM clojure:openjdk-17-tools-deps AS builder
WORKDIR /app
COPY deps.edn .
RUN clojure -P
COPY . .
RUN clojure -T:build uber

FROM openjdk:17-slim
WORKDIR /app
COPY --from=builder /app/target/*.jar app.jar
EXPOSE 8080
CMD ["java", "-jar", "app.jar"]
EOF
            ;;
        "erlang")
            cat > "${PATH}/Dockerfile" <<'EOF'
FROM erlang:26-alpine AS builder
RUN apk add --no-cache git make
WORKDIR /app
COPY rebar.config .
RUN rebar3 compile
COPY . .
RUN rebar3 release

FROM erlang:26-alpine
WORKDIR /app
COPY --from=builder /app/_build/default/rel/task_server ./
EXPOSE 8080
CMD ["bin/task_server", "foreground"]
EOF
            ;;
        "cpp")
            cat > "${PATH}/Dockerfile" <<'EOF'
FROM gcc:12 AS builder
RUN apt-get update && apt-get install -y cmake libboost-all-dev
WORKDIR /app
COPY CMakeLists.txt .
COPY src ./src
RUN cmake . && make

FROM debian:bookworm-slim
RUN apt-get update && apt-get install -y libstdc++6
WORKDIR /app
COPY --from=builder /app/task-server .
EXPOSE 8080
CMD ["./task-server"]
EOF
            ;;
        *)
            # Generic Dockerfile for interpreted languages
            cat > "${PATH}/Dockerfile" <<EOF
FROM alpine:latest
RUN apk add --no-cache ${LANG}
WORKDIR /app
COPY . .
EXPOSE 8080
CMD ["${LANG}", "server.${LANG}"]
EOF
            ;;
    esac
}

# Build REST services
echo -e "\n${YELLOW}Building REST Services...${NC}\n"

LANGUAGES=(
    "javascript" "typescript" "go" "python" "java" "rust" "kotlin" "swift"
    "csharp" "fsharp" "cpp" "scala" "ruby" "php" "dart" "crystal"
    "lua" "c" "perl" "r" "matlab" "fortran" "cobol" "ada"
    "clojure" "zig" "erlang" "ocaml" "nim" "julia" "gleam" "v" "io"
    "elixir" "haskell"
)

for lang in "${LANGUAGES[@]}"; do
    if [ -d "code/${lang}/rest/server" ]; then
        build_service "${lang}" "rest" "code/${lang}/rest/server"
    fi
done

# Build gRPC services
echo -e "\n${YELLOW}Building gRPC Services...${NC}\n"

GRPC_LANGUAGES=(
    "javascript" "typescript" "go" "python" "java" "rust" "kotlin" "swift"
    "csharp" "fsharp" "cpp" "scala" "ruby" "php" "dart" "perl"
    "clojure" "erlang" "elixir" "haskell"
)

for lang in "${GRPC_LANGUAGES[@]}"; do
    if [ -d "code/${lang}/grpc/server" ]; then
        build_service "${lang}" "grpc" "code/${lang}/grpc/server"
    fi
done

# Print summary
echo
echo "========================================"
echo "Build Summary"
echo "========================================"
echo -e "${GREEN}Successful builds: ${#SUCCESSFUL_BUILDS[@]}${NC}"
for build in "${SUCCESSFUL_BUILDS[@]}"; do
    echo "  ✓ $build"
done

if [ ${#FAILED_BUILDS[@]} -gt 0 ]; then
    echo -e "${RED}Failed builds: ${#FAILED_BUILDS[@]}${NC}"
    for build in "${FAILED_BUILDS[@]}"; do
        echo "  ✗ $build"
    done
fi

echo
echo "To run all services: docker-compose up"
echo "To run specific service: docker-compose up <service-name>"