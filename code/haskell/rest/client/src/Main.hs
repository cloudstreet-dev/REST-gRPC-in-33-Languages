{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (when)
import Data.Maybe (fromMaybe)
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import System.IO (hFlush, stdout)

import Client

main :: IO ()
main = do
    putStrLn "╔════════════════════════════════════════════════╗"
    putStrLn "║       Haskell Task Management REST Client      ║"
    putStrLn "║            Testing API Operations              ║"
    putStrLn "╚════════════════════════════════════════════════╝"
    putStrLn ""
    
    -- Create HTTP manager
    manager <- newManager tlsManagerSettings
    
    -- Demo operations
    runDemo manager
    
    putStrLn "\n✅ Demo completed successfully!"

runDemo :: Manager -> IO ()
runDemo manager = do
    -- 1. List all tasks
    putStrLn "1. Listing all tasks..."
    listResult <- listTasks manager
    case listResult of
        Left err -> putStrLn $ "   Error: " ++ err
        Right response -> do
            putStrLn $ "   Found " ++ show (totalCount response) ++ " tasks"
            mapM_ printTaskSummary (tasks response)
    
    -- 2. Create a new task
    putStrLn "\n2. Creating a new task..."
    let newTask = CreateTaskRequest
            { createTitle = "Learn Haskell monads"
            , createDescription = Just "Master monads, applicatives, and functors"
            , createPriority = Just "high"
            , createTags = Just ["haskell", "functional", "monads"]
            , createAssignedTo = Just "haskell-team"
            }
    createResult <- createTask manager newTask
    taskId <- case createResult of
        Left err -> do
            putStrLn $ "   Error: " ++ err
            return Nothing
        Right task -> do
            putStrLn $ "   Created task: " ++ show (taskTitle task)
            putStrLn $ "   ID: " ++ show (taskId task)
            putStrLn $ "   Priority: " ++ show (taskPriority task)
            putStrLn $ "   Tags: " ++ show (taskTags task)
            return $ Just (taskId task)
    
    -- Continue with other operations if task was created
    case taskId of
        Nothing -> putStrLn "   Skipping remaining operations due to creation failure"
        Just tid -> do
            -- 3. Get task details
            putStrLn "\n3. Getting task details..."
            getResult <- getTask manager tid
            case getResult of
                Left err -> putStrLn $ "   Error: " ++ err
                Right task -> do
                    putStrLn $ "   Title: " ++ show (taskTitle task)
                    putStrLn $ "   Description: " ++ show (taskDescription task)
                    putStrLn $ "   Status: " ++ show (taskStatus task)
                    putStrLn $ "   Assigned to: " ++ show (taskAssignedTo task)
            
            -- 4. Update task status
            putStrLn "\n4. Updating task status to 'in_progress'..."
            statusResult <- updateTaskStatus manager tid "in_progress"
            case statusResult of
                Left err -> putStrLn $ "   Error: " ++ err
                Right task -> putStrLn $ "   Updated status to: " ++ show (taskStatus task)
            
            -- 5. Update task details
            putStrLn "\n5. Updating task details..."
            let updates = UpdateTaskRequest
                    { updateTitle = Just "Master Haskell type system"
                    , updatePriority = Just "urgent"
                    }
            updateResult <- updateTask manager tid updates
            case updateResult of
                Left err -> putStrLn $ "   Error: " ++ err
                Right task -> do
                    putStrLn $ "   Updated title: " ++ show (taskTitle task)
                    putStrLn $ "   Updated priority: " ++ show (taskPriority task)
            
            -- 6. List tasks again
            putStrLn "\n6. Listing tasks again..."
            listResult2 <- listTasks manager
            case listResult2 of
                Left err -> putStrLn $ "   Error: " ++ err
                Right response -> do
                    putStrLn $ "   Total tasks: " ++ show (totalCount response)
                    let inProgress = filter (\t -> taskStatus t == "in_progress") (tasks response)
                    putStrLn $ "   In-progress tasks: " ++ show (length inProgress)
            
            -- 7. Delete the task
            putStrLn "\n7. Deleting the task..."
            deleteResult <- deleteTask manager tid
            case deleteResult of
                Left err -> putStrLn $ "   Error: " ++ err
                Right () -> putStrLn "   Task deleted successfully"
            
            -- 8. Verify deletion
            putStrLn "\n8. Verifying deletion..."
            verifyResult <- getTask manager tid
            case verifyResult of
                Left err | "not found" `elem` words (map toLower err) -> 
                    putStrLn "   Task not found (as expected)"
                Left err -> putStrLn $ "   Error: " ++ err
                Right _ -> putStrLn "   Error: Task still exists"

printTaskSummary :: Task -> IO ()
printTaskSummary task = do
    putStrLn $ "   - [" ++ show (taskId task) ++ "] " ++ 
               show (taskTitle task) ++ " (" ++ show (taskStatus task) ++ ")"

-- Helper function for case-insensitive character conversion
toLower :: Char -> Char
toLower c
    | c >= 'A' && c <= 'Z' = toEnum (fromEnum c + 32)
    | otherwise = c