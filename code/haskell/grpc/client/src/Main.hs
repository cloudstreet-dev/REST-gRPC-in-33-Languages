{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Main where

import Network.GRPC.HighLevel.Generated
import Network.GRPC.HighLevel.Client
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Monad
import System.Environment

-- Simplified types (would be generated from proto)
data Task = Task
    { taskId :: T.Text
    , taskTitle :: T.Text
    , taskDescription :: T.Text
    , taskStatus :: T.Text
    , taskPriority :: T.Text
    } deriving (Show)

data ListTasksRequest = ListTasksRequest
    { filterStatus :: Maybe T.Text
    , pageSize :: Int
    } deriving (Show)

data ListTasksResponse = ListTasksResponse
    { tasks :: [Task]
    , totalCount :: Int
    } deriving (Show)

data CreateTaskRequest = CreateTaskRequest
    { createTitle :: T.Text
    , createDescription :: T.Text
    , createPriority :: T.Text
    } deriving (Show)

-- Client functions
listTasks :: ClientConfig -> Maybe T.Text -> IO (Either GRPCError ListTasksResponse)
listTasks config statusFilter = do
    withGRPCClient config $ \client -> do
        let request = ListTasksRequest 
                { filterStatus = statusFilter
                , pageSize = 20
                }
        
        -- This would use generated client stub
        -- For now, returning a mock response
        return $ Right $ ListTasksResponse 
            { tasks = []
            , totalCount = 0
            }

createTask :: ClientConfig -> T.Text -> T.Text -> T.Text -> IO (Either GRPCError Task)
createTask config title desc priority = do
    withGRPCClient config $ \client -> do
        let request = CreateTaskRequest
                { createTitle = title
                , createDescription = desc
                , createPriority = priority
                }
        
        -- This would use generated client stub
        -- For now, returning a mock response
        return $ Right $ Task 
            { taskId = "task-123"
            , taskTitle = title
            , taskDescription = desc
            , taskStatus = "pending"
            , taskPriority = priority
            }

-- Main client application
main :: IO ()
main = do
    args <- getArgs
    
    let config = ClientConfig 
            { clientServerHost = "localhost"
            , clientServerPort = 50051
            , clientArgs = []
            , clientSSLConfig = Nothing
            , clientAuthority = Nothing
            }
    
    case args of
        ["list"] -> do
            putStrLn "Listing all tasks..."
            result <- listTasks config Nothing
            case result of
                Right response -> do
                    putStrLn $ "Found " ++ show (totalCount response) ++ " tasks"
                    forM_ (tasks response) $ \task -> do
                        TIO.putStrLn $ "- " <> taskTitle task <> " (" <> taskStatus task <> ")"
                Left err -> putStrLn $ "Error: " ++ show err
        
        ["list", status] -> do
            putStrLn $ "Listing tasks with status: " ++ status
            result <- listTasks config (Just $ T.pack status)
            case result of
                Right response -> do
                    putStrLn $ "Found " ++ show (totalCount response) ++ " tasks"
                    forM_ (tasks response) $ \task -> do
                        TIO.putStrLn $ "- " <> taskTitle task
                Left err -> putStrLn $ "Error: " ++ show err
        
        ["create", title, desc] -> do
            putStrLn "Creating task..."
            result <- createTask config (T.pack title) (T.pack desc) "medium"
            case result of
                Right task -> do
                    putStrLn $ "Created task: " ++ T.unpack (taskId task)
                    TIO.putStrLn $ "Title: " <> taskTitle task
                    TIO.putStrLn $ "Status: " <> taskStatus task
                Left err -> putStrLn $ "Error: " ++ show err
        
        _ -> do
            putStrLn "Usage:"
            putStrLn "  task-grpc-client list                    - List all tasks"
            putStrLn "  task-grpc-client list <status>          - List tasks by status"
            putStrLn "  task-grpc-client create <title> <desc>  - Create a new task"

-- Simplified client config (would use real grpc-haskell types)
data ClientConfig = ClientConfig
    { clientServerHost :: String
    , clientServerPort :: Int
    , clientArgs :: [String]
    , clientSSLConfig :: Maybe String
    , clientAuthority :: Maybe String
    }

data GRPCError = GRPCError String deriving (Show)

-- Simplified withGRPCClient (would use real implementation)
withGRPCClient :: ClientConfig -> (Client -> IO a) -> IO a
withGRPCClient config action = do
    -- This would establish actual gRPC connection
    let client = Client config
    action client

data Client = Client ClientConfig