{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module API where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON, FromJSON, encode, withObject, (.:))
import Data.ByteString.Lazy (toStrict)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack, unpack)
import Data.UUID (UUID)
import Data.UUID (fromString)
import Network.HTTP.Types.Status
import Servant

import Types
import Repository

-- API Type Definition
type TaskAPI = "health" :> Get '[JSON] HealthResponse
          :<|> "api" :> "tasks" :> QueryParam "status" Text
                                 :> QueryParam "assigned_to" Text
                                 :> QueryParam "tags" Text
                                 :> QueryParam "page_size" Int
                                 :> QueryParam "page_token" Int
                                 :> QueryParam "sort_by" Text
                                 :> QueryParam "sort_order" Text
                                 :> Get '[JSON] ListTasksResponse
          :<|> "api" :> "tasks" :> Capture "id" Text :> Get '[JSON] Task
          :<|> "api" :> "tasks" :> ReqBody '[JSON] CreateTaskRequest :> PostCreated '[JSON] Task
          :<|> "api" :> "tasks" :> Capture "id" Text :> ReqBody '[JSON] UpdateTaskRequest :> Put '[JSON] Task
          :<|> "api" :> "tasks" :> Capture "id" Text :> "status" 
                                 :> ReqBody '[JSON] StatusUpdateRequest :> Patch '[JSON] Task
          :<|> "api" :> "tasks" :> Capture "id" Text :> DeleteNoContent

-- Status Update Request
data StatusUpdateRequest = StatusUpdateRequest
    { newStatus :: TaskStatus
    } deriving (Show)

instance FromJSON StatusUpdateRequest where
    parseJSON = withObject "StatusUpdateRequest" $ \v ->
        StatusUpdateRequest <$> v .: "status"

-- API Handler
taskAPI :: Proxy TaskAPI
taskAPI = Proxy

-- Server implementation
server :: Repository -> Server TaskAPI
server repo = healthHandler
         :<|> listTasksHandler
         :<|> getTaskHandler
         :<|> createTaskHandler
         :<|> updateTaskHandler
         :<|> updateStatusHandler
         :<|> deleteTaskHandler
  where
    -- Health check
    healthHandler :: Handler HealthResponse
    healthHandler = do
        count <- liftIO $ getTaskCount repo
        return $ HealthResponse
            { healthStatus = "healthy"
            , service = "haskell-task-api"
            , taskCount = count
            }
    
    -- List tasks
    listTasksHandler :: Maybe Text -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int
                     -> Maybe Text -> Maybe Text -> Handler ListTasksResponse
    listTasksHandler statusParam assignedTo tagsParam pageSize pageToken sortBy sortOrder = do
        let statusFilter = statusParam >>= parseStatus
            tagsFilter = tagsParam >>= \t -> Just [t]  -- Simple parsing for demo
        liftIO $ listTasks repo statusFilter assignedTo tagsFilter pageSize pageToken sortBy sortOrder
    
    -- Get task
    getTaskHandler :: Text -> Handler Task
    getTaskHandler idText = do
        case fromString (unpack idText) of
            Nothing -> throwError err400 { errBody = toStrict $ encode $ ErrorResponse "Invalid UUID" }
            Just uuid -> do
                maybeTask <- liftIO $ getTask repo uuid
                case maybeTask of
                    Nothing -> throwError err404 { errBody = toStrict $ encode $ ErrorResponse "Task not found" }
                    Just task -> return task
    
    -- Create task
    createTaskHandler :: CreateTaskRequest -> Handler Task
    createTaskHandler req = do
        if createTitle req == ""
            then throwError err400 { errBody = toStrict $ encode $ ErrorResponse "Title is required" }
            else liftIO $ createTaskInRepo repo req
    
    -- Update task
    updateTaskHandler :: Text -> UpdateTaskRequest -> Handler Task
    updateTaskHandler idText req = do
        case fromString (unpack idText) of
            Nothing -> throwError err400 { errBody = toStrict $ encode $ ErrorResponse "Invalid UUID" }
            Just uuid -> do
                maybeTask <- liftIO $ updateTaskInRepo repo uuid req
                case maybeTask of
                    Nothing -> throwError err404 { errBody = toStrict $ encode $ ErrorResponse "Task not found" }
                    Just task -> return task
    
    -- Update status
    updateStatusHandler :: Text -> StatusUpdateRequest -> Handler Task
    updateStatusHandler idText req = do
        case fromString (unpack idText) of
            Nothing -> throwError err400 { errBody = toStrict $ encode $ ErrorResponse "Invalid UUID" }
            Just uuid -> do
                maybeTask <- liftIO $ updateTaskStatus repo uuid (newStatus req)
                case maybeTask of
                    Nothing -> throwError err404 { errBody = toStrict $ encode $ ErrorResponse "Task not found" }
                    Just task -> return task
    
    -- Delete task
    deleteTaskHandler :: Text -> Handler NoContent
    deleteTaskHandler idText = do
        case fromString (unpack idText) of
            Nothing -> throwError err400 { errBody = toStrict $ encode $ ErrorResponse "Invalid UUID" }
            Just uuid -> do
                deleted <- liftIO $ deleteTask repo uuid
                if deleted
                    then return NoContent
                    else throwError err404 { errBody = toStrict $ encode $ ErrorResponse "Task not found" }

-- Helper to parse status
parseStatus :: Text -> Maybe TaskStatus
parseStatus "pending" = Just Pending
parseStatus "in_progress" = Just InProgress
parseStatus "completed" = Just Completed
parseStatus "cancelled" = Just Cancelled
parseStatus _ = Nothing