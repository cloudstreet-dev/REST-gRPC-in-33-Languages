{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import Servant
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

import API
import Repository

main :: IO ()
main = do
    putStrLn "╔════════════════════════════════════════════════╗"
    putStrLn "║       Haskell Task Management REST API         ║"
    putStrLn "║            Built with Servant                  ║"
    putStrLn "╚════════════════════════════════════════════════╝"
    putStrLn ""
    
    -- Get port from environment
    portEnv <- lookupEnv "PORT"
    let port = maybe 8080 (fromMaybe 8080 . readMaybe) portEnv
    
    putStrLn $ "[INFO] Haskell Task REST Server starting on port " ++ show port
    putStrLn $ "[INFO] Visit http://localhost:" ++ show port ++ "/api/tasks"
    putStrLn ""
    putStrLn "Available endpoints:"
    putStrLn "  GET    /api/tasks          - List all tasks"
    putStrLn "  GET    /api/tasks/{id}     - Get a specific task"
    putStrLn "  POST   /api/tasks          - Create a new task"
    putStrLn "  PUT    /api/tasks/{id}     - Update a task"
    putStrLn "  PATCH  /api/tasks/{id}/status - Update task status"
    putStrLn "  DELETE /api/tasks/{id}     - Delete a task"
    putStrLn "  GET    /health             - Health check"
    putStrLn ""
    putStrLn "Sample requests:"
    putStrLn $ "  curl http://localhost:" ++ show port ++ "/api/tasks"
    putStrLn $ "  curl -X POST http://localhost:" ++ show port ++ "/api/tasks \\"
    putStrLn "    -H \"Content-Type: application/json\" \\"
    putStrLn "    -d '{\"title\":\"New Task\",\"priority\":\"high\"}'"
    putStrLn ""
    putStrLn "[INFO] Press Ctrl+C to stop the server"
    putStrLn ""
    
    -- Initialize repository
    repo <- newRepository
    
    -- Create WAI application
    let app = serve taskAPI (server repo)
    
    -- Add CORS middleware
    let corsPolicy = simpleCorsResourcePolicy
            { corsRequestHeaders = ["Content-Type"]
            , corsMethods = ["GET", "POST", "PUT", "PATCH", "DELETE", "OPTIONS"]
            }
    
    -- Run the server
    run port $ cors (const $ Just corsPolicy) app