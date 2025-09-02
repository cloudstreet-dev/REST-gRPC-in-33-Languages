cmake_minimum_required(VERSION 3.20)
project(TaskRestServer CXX)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Find required packages
find_package(Threads REQUIRED)
find_package(PkgConfig REQUIRED)

# Find Crow framework
find_package(Crow CONFIG)
if(NOT Crow_FOUND)
    include(FetchContent)
    FetchContent_Declare(
        Crow
        GIT_REPOSITORY https://github.com/CrowCpp/Crow.git
        GIT_TAG v1.0+5
    )
    FetchContent_MakeAvailable(Crow)
endif()

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

# Find SQLite
find_package(SQLite3)
if(NOT SQLite3_FOUND)
    include(FetchContent)
    FetchContent_Declare(
        sqlite3
        URL https://www.sqlite.org/2023/sqlite-amalgamation-3440200.zip
    )
    FetchContent_MakeAvailable(sqlite3)
    
    add_library(sqlite3 STATIC ${sqlite3_SOURCE_DIR}/sqlite3.c)
    target_include_directories(sqlite3 PUBLIC ${sqlite3_SOURCE_DIR})
    target_compile_definitions(sqlite3 PUBLIC SQLITE_ENABLE_JSON1)
endif()

# Add executable
add_executable(task_rest_server
    src/main.cpp
    src/models.cpp
    src/database.cpp
    src/handlers.cpp
)

# Link libraries
target_link_libraries(task_rest_server 
    PRIVATE 
    Crow::Crow
    nlohmann_json::nlohmann_json
    Threads::Threads
)

if(SQLite3_FOUND)
    target_link_libraries(task_rest_server PRIVATE SQLite::SQLite3)
else()
    target_link_libraries(task_rest_server PRIVATE sqlite3)
endif()

# Include directories
target_include_directories(task_rest_server PRIVATE include)

# Compiler options
target_compile_options(task_rest_server PRIVATE
    -Wall
    -Wextra
    -O2
)