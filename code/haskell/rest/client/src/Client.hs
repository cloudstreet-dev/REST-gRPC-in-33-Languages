{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Client where

import Control.Exception (try, SomeException)
import Data.Aeson
import Data.ByteString.Lazy.Char8 as L8
import Data.Text (Text)
import GHC.Generics
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Status

-- Reuse types from server (simplified for client)
data Task = Task
    { taskId :: Text
    , taskTitle :: Text
    , taskDescription :: Text
    , taskStatus :: Text
    , taskPriority :: Text
    , taskTags :: [Text]
    , taskAssignedTo :: Text
    , taskCreatedAt :: Text
    , taskUpdatedAt :: Text
    } deriving (Show, Generic)

instance FromJSON Task where
    parseJSON = withObject "Task" $ \v -> Task
        <$> v .: "id"
        <*> v .: "title"
        <*> v .: "description"
        <*> v .: "status"
        <*> v .: "priority"
        <*> v .: "tags"
        <*> v .: "assigned_to"
        <*> v .: "created_at"
        <*> v .: "updated_at"

data ListTasksResponse = ListTasksResponse
    { tasks :: [Task]
    , totalCount :: Int
    , pageSize :: Int
    , nextPageToken :: Maybe Text
    } deriving (Show, Generic)

instance FromJSON ListTasksResponse where
    parseJSON = withObject "ListTasksResponse" $ \v -> ListTasksResponse
        <$> v .: "tasks"
        <*> v .: "total_count"
        <*> v .: "page_size"
        <*> v .:? "next_page_token"

data CreateTaskRequest = CreateTaskRequest
    { createTitle :: Text
    , createDescription :: Maybe Text
    , createPriority :: Maybe Text
    , createTags :: Maybe [Text]
    , createAssignedTo :: Maybe Text
    } deriving (Show, Generic)

instance ToJSON CreateTaskRequest where
    toJSON req = object
        [ "title" .= createTitle req
        , "description" .= createDescription req
        , "priority" .= createPriority req
        , "tags" .= createTags req
        , "assigned_to" .= createAssignedTo req
        ]

data UpdateTaskRequest = UpdateTaskRequest
    { updateTitle :: Maybe Text
    , updatePriority :: Maybe Text
    } deriving (Show, Generic)

instance ToJSON UpdateTaskRequest where
    toJSON req = object
        [ "title" .= updateTitle req
        , "priority" .= updatePriority req
        ]

data StatusUpdateRequest = StatusUpdateRequest
    { status :: Text
    } deriving (Show, Generic)

instance ToJSON StatusUpdateRequest

-- Client functions
baseUrl :: String
baseUrl = "http://localhost:8080/api"

-- List all tasks
listTasks :: Manager -> IO (Either String ListTasksResponse)
listTasks manager = do
    request <- parseRequest $ baseUrl ++ "/tasks"
    response <- httpLbs request manager
    let statusCode = statusCode $ responseStatus response
    if statusCode == 200
        then case eitherDecode (responseBody response) of
            Right tasks -> return $ Right tasks
            Left err -> return $ Left $ "Failed to parse response: " ++ err
        else return $ Left $ "Request failed with status: " ++ show statusCode

-- Get a specific task
getTask :: Manager -> Text -> IO (Either String Task)
getTask manager taskId = do
    request <- parseRequest $ baseUrl ++ "/tasks/" ++ show taskId
    response <- httpLbs request manager
    let statusCode = statusCode $ responseStatus response
    if statusCode == 200
        then case eitherDecode (responseBody response) of
            Right task -> return $ Right task
            Left err -> return $ Left $ "Failed to parse response: " ++ err
        else if statusCode == 404
            then return $ Left "Task not found"
            else return $ Left $ "Request failed with status: " ++ show statusCode

-- Create a new task
createTask :: Manager -> CreateTaskRequest -> IO (Either String Task)
createTask manager req = do
    initialRequest <- parseRequest $ baseUrl ++ "/tasks"
    let request = initialRequest
            { method = "POST"
            , requestBody = RequestBodyLBS $ encode req
            , requestHeaders = [("Content-Type", "application/json")]
            }
    response <- httpLbs request manager
    let statusCode = statusCode $ responseStatus response
    if statusCode == 201
        then case eitherDecode (responseBody response) of
            Right task -> return $ Right task
            Left err -> return $ Left $ "Failed to parse response: " ++ err
        else return $ Left $ "Request failed with status: " ++ show statusCode

-- Update a task
updateTask :: Manager -> Text -> UpdateTaskRequest -> IO (Either String Task)
updateTask manager taskId req = do
    initialRequest <- parseRequest $ baseUrl ++ "/tasks/" ++ show taskId
    let request = initialRequest
            { method = "PUT"
            , requestBody = RequestBodyLBS $ encode req
            , requestHeaders = [("Content-Type", "application/json")]
            }
    response <- httpLbs request manager
    let statusCode = statusCode $ responseStatus response
    if statusCode == 200
        then case eitherDecode (responseBody response) of
            Right task -> return $ Right task
            Left err -> return $ Left $ "Failed to parse response: " ++ err
        else return $ Left $ "Request failed with status: " ++ show statusCode

-- Update task status
updateTaskStatus :: Manager -> Text -> Text -> IO (Either String Task)
updateTaskStatus manager taskId newStatus = do
    initialRequest <- parseRequest $ baseUrl ++ "/tasks/" ++ show taskId ++ "/status"
    let request = initialRequest
            { method = "PATCH"
            , requestBody = RequestBodyLBS $ encode $ StatusUpdateRequest newStatus
            , requestHeaders = [("Content-Type", "application/json")]
            }
    response <- httpLbs request manager
    let statusCode = statusCode $ responseStatus response
    if statusCode == 200
        then case eitherDecode (responseBody response) of
            Right task -> return $ Right task
            Left err -> return $ Left $ "Failed to parse response: " ++ err
        else return $ Left $ "Request failed with status: " ++ show statusCode

-- Delete a task
deleteTask :: Manager -> Text -> IO (Either String ())
deleteTask manager taskId = do
    initialRequest <- parseRequest $ baseUrl ++ "/tasks/" ++ show taskId
    let request = initialRequest { method = "DELETE" }
    response <- httpLbs request manager
    let statusCode = statusCode $ responseStatus response
    if statusCode == 204
        then return $ Right ()
        else if statusCode == 404
            then return $ Left "Task not found"
            else return $ Left $ "Request failed with status: " ++ show statusCode