from typing import Dict, List, Optional
from uuid import UUID
import asyncio
from datetime import datetime

from models import (
    Task, TaskStatus, TaskPriority, SortOrder,
    CreateTaskRequest, UpdateTaskRequest,
    ListTasksQuery, ListTasksResponse
)


class TaskService:
    def __init__(self):
        self.tasks: Dict[str, Task] = {}
        self.lock = asyncio.Lock()
        self._initialize_sample_data()
    
    def _initialize_sample_data(self):
        """Initialize with sample tasks"""
        sample_tasks = [
            CreateTaskRequest(
                title="Implement Python REST API",
                description="Create REST API using FastAPI framework",
                priority=TaskPriority.HIGH,
                tags=["python", "rest", "api"]
            ),
            CreateTaskRequest(
                title="Add gRPC support",
                description="Implement gRPC server with Python",
                priority=TaskPriority.MEDIUM,
                tags=["python", "grpc", "protobuf"]
            ),
            CreateTaskRequest(
                title="Write unit tests",
                description="Add pytest tests for API endpoints",
                priority=TaskPriority.HIGH,
                tags=["python", "testing", "pytest"]
            )
        ]
        
        for task_data in sample_tasks:
            task = Task(
                title=task_data.title,
                description=task_data.description,
                priority=task_data.priority,
                tags=task_data.tags
            )
            self.tasks[str(task.id)] = task
    
    async def list_tasks(self, query: ListTasksQuery) -> ListTasksResponse:
        """List tasks with filtering, sorting, and pagination"""
        async with self.lock:
            # Filter tasks
            filtered_tasks = [
                task for task in self.tasks.values()
                if task.matches_filters(
                    status=query.status,
                    assigned_to=query.assigned_to,
                    tags=query.tags
                )
            ]
            
            # Sort tasks
            sorted_tasks = self._sort_tasks(filtered_tasks, query.sort_order)
            
            # Paginate
            start_index = int(query.page_token) if query.page_token else 0
            end_index = start_index + query.page_size
            
            tasks_page = sorted_tasks[start_index:end_index]
            next_page_token = str(end_index) if end_index < len(sorted_tasks) else None
            
            return ListTasksResponse(
                tasks=tasks_page,
                next_page_token=next_page_token,
                total_count=len(sorted_tasks)
            )
    
    async def get_task(self, task_id: str) -> Task:
        """Get a single task by ID"""
        async with self.lock:
            task = self.tasks.get(task_id)
            if not task:
                raise ValueError(f"Task with ID {task_id} not found")
            return task
    
    async def create_task(self, request: CreateTaskRequest) -> Task:
        """Create a new task"""
        task = Task(
            title=request.title,
            description=request.description or "",
            priority=request.priority or TaskPriority.MEDIUM,
            tags=request.tags or [],
            assigned_to=request.assigned_to,
            due_date=request.due_date
        )
        
        async with self.lock:
            self.tasks[str(task.id)] = task
        
        return task
    
    async def update_task(self, task_id: str, request: UpdateTaskRequest) -> Task:
        """Update an existing task"""
        async with self.lock:
            task = self.tasks.get(task_id)
            if not task:
                raise ValueError(f"Task with ID {task_id} not found")
            
            # Update only provided fields
            update_data = request.dict(exclude_unset=True)
            task.update(**update_data)
            
            return task
    
    async def update_task_status(self, task_id: str, status: TaskStatus) -> Task:
        """Update task status"""
        async with self.lock:
            task = self.tasks.get(task_id)
            if not task:
                raise ValueError(f"Task with ID {task_id} not found")
            
            task.update(status=status)
            return task
    
    async def delete_task(self, task_id: str) -> None:
        """Delete a task"""
        async with self.lock:
            if task_id not in self.tasks:
                raise ValueError(f"Task with ID {task_id} not found")
            
            del self.tasks[task_id]
    
    def _sort_tasks(self, tasks: List[Task], sort_order: Optional[SortOrder]) -> List[Task]:
        """Sort tasks based on the specified order"""
        if not sort_order:
            return tasks
        
        if sort_order == SortOrder.CREATED_AT_ASC:
            return sorted(tasks, key=lambda t: t.created_at)
        elif sort_order == SortOrder.CREATED_AT_DESC:
            return sorted(tasks, key=lambda t: t.created_at, reverse=True)
        elif sort_order == SortOrder.DUE_DATE_ASC:
            return sorted(tasks, key=lambda t: t.due_date or datetime.max)
        elif sort_order == SortOrder.DUE_DATE_DESC:
            return sorted(tasks, key=lambda t: t.due_date or datetime.min, reverse=True)
        elif sort_order == SortOrder.PRIORITY_ASC:
            return sorted(tasks, key=lambda t: t.priority.get_value())
        elif sort_order == SortOrder.PRIORITY_DESC:
            return sorted(tasks, key=lambda t: t.priority.get_value(), reverse=True)
        
        return tasks