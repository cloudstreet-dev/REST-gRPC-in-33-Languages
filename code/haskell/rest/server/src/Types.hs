{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import Data.Aeson
import Data.Text (Text)
import Data.Time (UTCTime, getCurrentTime)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import GHC.Generics

-- Task Status
data TaskStatus = Pending | InProgress | Completed | Cancelled
    deriving (Show, Eq, Generic)

instance ToJSON TaskStatus where
    toJSON Pending = String "pending"
    toJSON InProgress = String "in_progress"
    toJSON Completed = String "completed"
    toJSON Cancelled = String "cancelled"

instance FromJSON TaskStatus where
    parseJSON (String "pending") = pure Pending
    parseJSON (String "in_progress") = pure InProgress
    parseJSON (String "completed") = pure Completed
    parseJSON (String "cancelled") = pure Cancelled
    parseJSON _ = fail "Invalid task status"

-- Task Priority
data TaskPriority = Low | Medium | High | Urgent
    deriving (Show, Eq, Generic)

instance ToJSON TaskPriority where
    toJSON Low = String "low"
    toJSON Medium = String "medium"
    toJSON High = String "high"
    toJSON Urgent = String "urgent"

instance FromJSON TaskPriority where
    parseJSON (String "low") = pure Low
    parseJSON (String "medium") = pure Medium
    parseJSON (String "high") = pure High
    parseJSON (String "urgent") = pure Urgent
    parseJSON _ = fail "Invalid task priority"

-- Task
data Task = Task
    { taskId :: UUID
    , taskTitle :: Text
    , taskDescription :: Text
    , taskStatus :: TaskStatus
    , taskPriority :: TaskPriority
    , taskTags :: [Text]
    , taskAssignedTo :: Text
    , taskCreatedAt :: UTCTime
    , taskUpdatedAt :: UTCTime
    } deriving (Show, Eq, Generic)

instance ToJSON Task where
    toJSON task = object
        [ "id" .= taskId task
        , "title" .= taskTitle task
        , "description" .= taskDescription task
        , "status" .= taskStatus task
        , "priority" .= taskPriority task
        , "tags" .= taskTags task
        , "assigned_to" .= taskAssignedTo task
        , "created_at" .= taskCreatedAt task
        , "updated_at" .= taskUpdatedAt task
        ]

instance FromJSON Task where
    parseJSON = withObject "Task" $ \v -> Task
        <$> v .: "id"
        <*> v .: "title"
        <*> v .:? "description" .!= ""
        <*> v .:? "status" .!= Pending
        <*> v .:? "priority" .!= Medium
        <*> v .:? "tags" .!= []
        <*> v .:? "assigned_to" .!= ""
        <*> v .: "created_at"
        <*> v .: "updated_at"

-- Create Task Request
data CreateTaskRequest = CreateTaskRequest
    { createTitle :: Text
    , createDescription :: Maybe Text
    , createStatus :: Maybe TaskStatus
    , createPriority :: Maybe TaskPriority
    , createTags :: Maybe [Text]
    , createAssignedTo :: Maybe Text
    } deriving (Show, Generic)

instance FromJSON CreateTaskRequest where
    parseJSON = withObject "CreateTaskRequest" $ \v -> CreateTaskRequest
        <$> v .: "title"
        <*> v .:? "description"
        <*> v .:? "status"
        <*> v .:? "priority"
        <*> v .:? "tags"
        <*> v .:? "assigned_to"

-- Update Task Request
data UpdateTaskRequest = UpdateTaskRequest
    { updateTitle :: Maybe Text
    , updateDescription :: Maybe Text
    , updateStatus :: Maybe TaskStatus
    , updatePriority :: Maybe TaskPriority
    , updateTags :: Maybe [Text]
    , updateAssignedTo :: Maybe Text
    } deriving (Show, Generic)

instance FromJSON UpdateTaskRequest where
    parseJSON = withObject "UpdateTaskRequest" $ \v -> UpdateTaskRequest
        <$> v .:? "title"
        <*> v .:? "description"
        <*> v .:? "status"
        <*> v .:? "priority"
        <*> v .:? "tags"
        <*> v .:? "assigned_to"

-- List Tasks Response
data ListTasksResponse = ListTasksResponse
    { tasks :: [Task]
    , totalCount :: Int
    , pageSize :: Int
    , nextPageToken :: Maybe Text
    } deriving (Show, Generic)

instance ToJSON ListTasksResponse where
    toJSON response = object
        [ "tasks" .= tasks response
        , "total_count" .= totalCount response
        , "page_size" .= pageSize response
        , "next_page_token" .= nextPageToken response
        ]

-- Health Response
data HealthResponse = HealthResponse
    { healthStatus :: Text
    , service :: Text
    , taskCount :: Int
    } deriving (Show, Generic)

instance ToJSON HealthResponse where
    toJSON health = object
        [ "status" .= healthStatus health
        , "service" .= service health
        , "task_count" .= taskCount health
        ]

-- Error Response
data ErrorResponse = ErrorResponse
    { errorMessage :: Text
    } deriving (Show, Generic)

instance ToJSON ErrorResponse where
    toJSON err = object [ "error" .= errorMessage err ]

-- Helper function to create a new task
createTask :: CreateTaskRequest -> IO Task
createTask req = do
    taskId' <- nextRandom
    now <- getCurrentTime
    return Task
        { taskId = taskId'
        , taskTitle = createTitle req
        , taskDescription = maybe "" id (createDescription req)
        , taskStatus = maybe Pending id (createStatus req)
        , taskPriority = maybe Medium id (createPriority req)
        , taskTags = maybe [] id (createTags req)
        , taskAssignedTo = maybe "" id (createAssignedTo req)
        , taskCreatedAt = now
        , taskUpdatedAt = now
        }

-- Helper function to update a task
updateTask :: Task -> UpdateTaskRequest -> IO Task
updateTask task req = do
    now <- getCurrentTime
    return task
        { taskTitle = maybe (taskTitle task) id (updateTitle req)
        , taskDescription = maybe (taskDescription task) id (updateDescription req)
        , taskStatus = maybe (taskStatus task) id (updateStatus req)
        , taskPriority = maybe (taskPriority task) id (updatePriority req)
        , taskTags = maybe (taskTags task) id (updateTags req)
        , taskAssignedTo = maybe (taskAssignedTo task) id (updateAssignedTo req)
        , taskUpdatedAt = now
        }