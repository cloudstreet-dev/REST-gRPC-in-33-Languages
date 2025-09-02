#!/usr/bin/env julia

# Add package to load path
push!(LOAD_PATH, @__DIR__)

using TaskServer

# Start the server
TaskServer.start_server(8080)