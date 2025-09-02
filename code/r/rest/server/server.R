#!/usr/bin/env Rscript

# Load required libraries
library(plumber)

# Get port from environment or use default
port <- as.numeric(Sys.getenv("PORT", 8080))

# Create and run the API
pr <- plumber::plumb("api.R")
pr$run(port = port, host = "0.0.0.0")