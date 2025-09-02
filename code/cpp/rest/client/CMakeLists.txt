cmake_minimum_required(VERSION 3.20)
project(TaskRestClient CXX)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Find required packages
find_package(Threads REQUIRED)
find_package(CURL REQUIRED)

# Find nlohmann/json
find_package(nlohmann_json CONFIG)
if(NOT nlohmann_json_FOUND)
    include(FetchContent)
    FetchContent_Declare(
        nlohmann_json
        URL https://github.com/nlohmann/json/releases/download/v3.11.2/json.tar.xz
    )
    FetchContent_MakeAvailable(nlohmann_json)
endif()

# Find CLI11
find_package(CLI11 CONFIG)
if(NOT CLI11_FOUND)
    include(FetchContent)
    FetchContent_Declare(
        CLI11
        GIT_REPOSITORY https://github.com/CLIUtils/CLI11.git
        GIT_TAG v2.3.2
    )
    FetchContent_MakeAvailable(CLI11)
endif()

# Add executable
add_executable(task_rest_client
    src/main.cpp
    src/models.cpp
    src/api_client.cpp
)

# Link libraries
target_link_libraries(task_rest_client 
    PRIVATE 
    CURL::libcurl
    nlohmann_json::nlohmann_json
    CLI11::CLI11
    Threads::Threads
)

# Include directories
target_include_directories(task_rest_client PRIVATE include)

# Compiler options
target_compile_options(task_rest_client PRIVATE
    -Wall
    -Wextra
    -O2
)