cmake_minimum_required(VERSION 3.20)
project(TaskGrpcClient CXX)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Find required packages
find_package(Threads REQUIRED)
find_package(Protobuf REQUIRED)
find_package(gRPC REQUIRED)

# Generate protobuf and gRPC files
set(PROTO_FILES ../../../shared/protos/tasks.proto)

protobuf_generate_cpp(PROTO_SRCS PROTO_HDRS ${PROTO_FILES})
grpc_generate_cpp(GRPC_SRCS GRPC_HDRS ${CMAKE_CURRENT_BINARY_DIR} ${PROTO_FILES})

# Add executable
add_executable(task_grpc_client
    src/main.cpp
    src/task_client.cpp
    ${PROTO_SRCS}
    ${GRPC_SRCS}
)

# Link libraries
target_link_libraries(task_grpc_client 
    PRIVATE 
    gRPC::grpc++
    protobuf::libprotobuf
    Threads::Threads
)

# Include directories
target_include_directories(task_grpc_client PRIVATE 
    include
    ${CMAKE_CURRENT_BINARY_DIR}  # For generated protobuf files
)

# Compiler options
target_compile_options(task_grpc_client PRIVATE
    -Wall
    -Wextra
    -O2
)