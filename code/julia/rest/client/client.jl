#!/usr/bin/env julia

# Add package to load path
push!(LOAD_PATH, @__DIR__)

using TaskClient

# Run the demo
TaskClient.print_banner()
sleep(1)  # Give server time to be ready
TaskClient.run_demo()