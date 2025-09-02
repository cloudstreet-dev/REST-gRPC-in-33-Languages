{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Network.GRPC.HighLevel.Generated
import Network.GRPC.HighLevel.Server
import qualified Data.Text as T
import Data.Time
import Data.UUID
import Data.UUID.V4
import Control.Concurrent.STM
import Control.Monad.IO.Class

-- Import generated protobuf types (would be generated from task.proto)
-- For this example, we'll define simplified types

data Task = Task
    { taskId :: T.Text
    , taskTitle :: T.Text
    , taskDescription :: T.Text
    , taskStatus :: T.Text
    , taskPriority :: T.Text
    , taskTags :: [T.Text]
    , taskAssignedTo :: T.Text
    , taskCreatedAt :: UTCTime
    , taskUpdatedAt :: UTCTime
    } deriving (Show, Eq)

data ListTasksRequest = ListTasksRequest
    { filterStatus :: Maybe T.Text
    , filterAssignedTo :: Maybe T.Text
    , filterTags :: [T.Text]
    , pageSize :: Int
    } deriving (Show, Eq)

data ListTasksResponse = ListTasksResponse
    { tasks :: [Task]
    , totalCount :: Int
    } deriving (Show, Eq)

data GetTaskRequest = GetTaskRequest
    { requestId :: T.Text
    } deriving (Show, Eq)

data CreateTaskRequest = CreateTaskRequest
    { createTitle :: T.Text
    , createDescription :: T.Text
    , createPriority :: T.Text
    , createTags :: [T.Text]
    , createAssignedTo :: T.Text
    } deriving (Show, Eq)

-- Task repository using STM
type TaskRepository = TVar [(T.Text, Task)]

-- Service handlers
listTasksHandler :: TaskRepository 
                 -> ServerRequest 'Normal ListTasksRequest ListTasksResponse 
                 -> IO (ServerResponse 'Normal ListTasksResponse)
listTasksHandler repo (ServerNormalRequest _metadata request) = do
    allTasks <- atomically $ readTVar repo
    
    let filtered = filterTasks allTasks (filterStatus request) 
                                        (filterAssignedTo request) 
                                        (filterTags request)
    let paginated = take (pageSize request) filtered
    
    let response = ListTasksResponse 
            { tasks = map snd paginated
            , totalCount = length paginated
            }
    
    return $ ServerNormalResponse response [] StatusOk ""

getTaskHandler :: TaskRepository 
               -> ServerRequest 'Normal GetTaskRequest Task 
               -> IO (ServerResponse 'Normal Task)
getTaskHandler repo (ServerNormalRequest _metadata request) = do
    allTasks <- atomically $ readTVar repo
    
    case lookup (requestId request) allTasks of
        Just task -> return $ ServerNormalResponse task [] StatusOk ""
        Nothing -> return $ ServerNormalResponse 
                        (error "Task not found") 
                        [] 
                        StatusNotFound 
                        "Task not found"

createTaskHandler :: TaskRepository 
                  -> ServerRequest 'Normal CreateTaskRequest Task 
                  -> IO (ServerResponse 'Normal Task)
createTaskHandler repo (ServerNormalRequest _metadata request) = do
    taskId <- T.pack . toString <$> nextRandom
    now <- getCurrentTime
    
    let task = Task 
            { taskId = taskId
            , taskTitle = createTitle request
            , taskDescription = createDescription request
            , taskStatus = "pending"
            , taskPriority = createPriority request
            , taskTags = createTags request
            , taskAssignedTo = createAssignedTo request
            , taskCreatedAt = now
            , taskUpdatedAt = now
            }
    
    atomically $ modifyTVar repo ((taskId, task) :)
    
    return $ ServerNormalResponse task [] StatusOk ""

-- Helper functions
filterTasks :: [(T.Text, Task)] 
            -> Maybe T.Text 
            -> Maybe T.Text 
            -> [T.Text] 
            -> [(T.Text, Task)]
filterTasks tasks statusFilter assignedFilter tagsFilter =
    filter (matchesFilters . snd) tasks
  where
    matchesFilters task = 
        matchesStatus task && matchesAssigned task && matchesTags task
    
    matchesStatus task = case statusFilter of
        Nothing -> True
        Just s -> taskStatus task == s
    
    matchesAssigned task = case assignedFilter of
        Nothing -> True
        Just a -> taskAssignedTo task == a
    
    matchesTags task = null tagsFilter || 
                       any (`elem` taskTags task) tagsFilter

toString :: UUID -> String
toString = show

-- Initialize repository with sample data
initRepository :: IO TaskRepository
initRepository = do
    now <- getCurrentTime
    let sampleTasks = 
            [ ("task-1", Task "task-1" "Learn Haskell" "Master functional programming" 
                             "in_progress" "high" ["haskell", "learning"] 
                             "developer" now now)
            , ("task-2", Task "task-2" "Build gRPC server" "Implement task service" 
                             "pending" "medium" ["grpc", "api"] 
                             "backend-team" now now)
            ]
    atomically $ newTVar sampleTasks

-- Main server
main :: IO ()
main = do
    putStrLn "Starting Haskell gRPC server on port 50051..."
    
    repo <- initRepository
    
    let handlers = TaskServiceHandlers
            { listTasksHandler = listTasksHandler repo
            , getTaskHandler = getTaskHandler repo
            , createTaskHandler = createTaskHandler repo
            }
    
    let serverConfig = ServerConfig 
            { serverHost = "localhost"
            , serverPort = 50051
            , serverArgs = 
                [ CompressionAlgArg GrpcCompressDeflate
                , MaxReceiveMessageLength maxBound
                ]
            , serverSecurity = Nothing
            }
    
    -- Note: In a real implementation, this would use generated service definitions
    runServer serverConfig handlers
    
    putStrLn "Server stopped"

-- Simplified service definition (would be generated from proto file)
data TaskServiceHandlers = TaskServiceHandlers
    { listTasksHandler :: ServerRequest 'Normal ListTasksRequest ListTasksResponse 
                       -> IO (ServerResponse 'Normal ListTasksResponse)
    , getTaskHandler :: ServerRequest 'Normal GetTaskRequest Task 
                     -> IO (ServerResponse 'Normal Task)
    , createTaskHandler :: ServerRequest 'Normal CreateTaskRequest Task 
                        -> IO (ServerResponse 'Normal Task)
    }

-- Simplified runServer (would use generated code)
runServer :: ServerConfig -> TaskServiceHandlers -> IO ()
runServer config handlers = do
    -- This is a simplified version
    -- Real implementation would use grpc-haskell's server functions
    putStrLn $ "Server running on " ++ serverHost config ++ ":" ++ show (serverPort config)
    -- Event loop would go here
    return ()