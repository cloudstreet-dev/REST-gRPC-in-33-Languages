{-# LANGUAGE OverloadedStrings #-}

module Repository where

import Control.Concurrent.STM
import Control.Monad (forM)
import Data.List (sortBy)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Ord (comparing, Down(..))
import Data.Text (Text, isInfixOf, pack)
import Data.Time (getCurrentTime)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import qualified Data.Map.Strict as Map

import Types

type TaskMap = Map.Map UUID Task
type Repository = TVar TaskMap

-- Create a new repository
newRepository :: IO Repository
newRepository = do
    repo <- newTVarIO Map.empty
    loadSampleData repo
    return repo

-- Load sample data
loadSampleData :: Repository -> IO ()
loadSampleData repo = do
    -- Sample task 1
    task1 <- createTask $ CreateTaskRequest
        { createTitle = "Implement Haskell REST API"
        , createDescription = Just "Build a REST API server using Servant framework"
        , createStatus = Just InProgress
        , createPriority = Just High
        , createTags = Just ["haskell", "rest", "api"]
        , createAssignedTo = Just "backend-team"
        }
    
    -- Sample task 2
    task2 <- createTask $ CreateTaskRequest
        { createTitle = "Add property testing"
        , createDescription = Just "Implement property-based tests with QuickCheck"
        , createStatus = Just Pending
        , createPriority = Just Medium
        , createTags = Just ["haskell", "testing", "quickcheck"]
        , createAssignedTo = Just "qa-team"
        }
    
    -- Sample task 3
    task3 <- createTask $ CreateTaskRequest
        { createTitle = "Write Hspec tests"
        , createDescription = Just "Add comprehensive test coverage using Hspec"
        , createStatus = Just Pending
        , createPriority = Just High
        , createTags = Just ["testing", "quality"]
        , createAssignedTo = Just "qa-team"
        }
    
    atomically $ do
        taskMap <- readTVar repo
        let taskMap' = Map.insert (taskId task1) task1 $
                      Map.insert (taskId task2) task2 $
                      Map.insert (taskId task3) task3 taskMap
        writeTVar repo taskMap'

-- List tasks with filtering
listTasks :: Repository -> Maybe TaskStatus -> Maybe Text -> Maybe [Text] 
          -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> IO ListTasksResponse
listTasks repo statusFilter assignedToFilter tagsFilter pageSize pageToken sortBy' sortOrder = do
    taskMap <- readTVarIO repo
    let allTasks = Map.elems taskMap
        
        -- Apply filters
        filteredTasks = filter (applyFilters statusFilter assignedToFilter tagsFilter) allTasks
        
        -- Sort tasks
        sortedTasks = sortTasks filteredTasks (fromMaybe "created_at" sortBy') (fromMaybe "desc" sortOrder)
        
        -- Pagination
        pageSize' = min 100 (fromMaybe 20 pageSize)
        offset = fromMaybe 0 pageToken
        paginatedTasks = take pageSize' $ drop offset sortedTasks
        totalCount' = length filteredTasks
        nextToken = if offset + pageSize' < totalCount'
                   then Just (pack $ show (offset + pageSize'))
                   else Nothing
    
    return $ ListTasksResponse
        { tasks = paginatedTasks
        , totalCount = totalCount'
        , pageSize = pageSize'
        , nextPageToken = nextToken
        }

-- Apply filters to a task
applyFilters :: Maybe TaskStatus -> Maybe Text -> Maybe [Text] -> Task -> Bool
applyFilters statusFilter assignedToFilter tagsFilter task =
    statusMatch && assignedToMatch && tagsMatch
  where
    statusMatch = case statusFilter of
        Nothing -> True
        Just status -> taskStatus task == status
    
    assignedToMatch = case assignedToFilter of
        Nothing -> True
        Just assignedTo -> taskAssignedTo task == assignedTo
    
    tagsMatch = case tagsFilter of
        Nothing -> True
        Just tags -> all (`elem` taskTags task) tags

-- Sort tasks
sortTasks :: [Task] -> Text -> Text -> [Task]
sortTasks tasks sortBy' sortOrder =
    let compareFn = case sortBy' of
            "title" -> comparing taskTitle
            "updated_at" -> comparing taskUpdatedAt
            _ -> comparing taskCreatedAt  -- Default to created_at
        
        sorted = sortBy compareFn tasks
    in if sortOrder == "asc" then sorted else reverse sorted

-- Get a single task
getTask :: Repository -> UUID -> IO (Maybe Task)
getTask repo taskId' = do
    taskMap <- readTVarIO repo
    return $ Map.lookup taskId' taskMap

-- Create a new task
createTaskInRepo :: Repository -> CreateTaskRequest -> IO Task
createTaskInRepo repo req = do
    newTask <- createTask req
    atomically $ do
        taskMap <- readTVar repo
        writeTVar repo $ Map.insert (taskId newTask) newTask taskMap
    return newTask

-- Update a task
updateTaskInRepo :: Repository -> UUID -> UpdateTaskRequest -> IO (Maybe Task)
updateTaskInRepo repo taskId' req = do
    maybeTask <- getTask repo taskId'
    case maybeTask of
        Nothing -> return Nothing
        Just task -> do
            updatedTask <- updateTask task req
            atomically $ do
                taskMap <- readTVar repo
                writeTVar repo $ Map.insert taskId' updatedTask taskMap
            return $ Just updatedTask

-- Update task status
updateTaskStatus :: Repository -> UUID -> TaskStatus -> IO (Maybe Task)
updateTaskStatus repo taskId' newStatus = do
    let req = UpdateTaskRequest
            { updateTitle = Nothing
            , updateDescription = Nothing
            , updateStatus = Just newStatus
            , updatePriority = Nothing
            , updateTags = Nothing
            , updateAssignedTo = Nothing
            }
    updateTaskInRepo repo taskId' req

-- Delete a task
deleteTask :: Repository -> UUID -> IO Bool
deleteTask repo taskId' = atomically $ do
    taskMap <- readTVar repo
    case Map.lookup taskId' taskMap of
        Nothing -> return False
        Just _ -> do
            writeTVar repo $ Map.delete taskId' taskMap
            return True

-- Get task count
getTaskCount :: Repository -> IO Int
getTaskCount repo = do
    taskMap <- readTVarIO repo
    return $ Map.size taskMap