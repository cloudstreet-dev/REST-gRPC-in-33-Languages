from datetime import datetime
from enum import Enum
from typing import List, Optional
from uuid import UUID, uuid4
from pydantic import BaseModel, Field, field_validator


class TaskStatus(str, Enum):
    PENDING = "pending"
    IN_PROGRESS = "in_progress"
    COMPLETED = "completed"
    CANCELLED = "cancelled"
    ON_HOLD = "on_hold"


class TaskPriority(str, Enum):
    LOW = "low"
    MEDIUM = "medium"
    HIGH = "high"
    CRITICAL = "critical"
    
    def get_value(self) -> int:
        priority_values = {
            TaskPriority.LOW: 1,
            TaskPriority.MEDIUM: 2,
            TaskPriority.HIGH: 3,
            TaskPriority.CRITICAL: 4
        }
        return priority_values.get(self, 2)


class SortOrder(str, Enum):
    CREATED_AT_ASC = "created_at_asc"
    CREATED_AT_DESC = "created_at_desc"
    DUE_DATE_ASC = "due_date_asc"
    DUE_DATE_DESC = "due_date_desc"
    PRIORITY_ASC = "priority_asc"
    PRIORITY_DESC = "priority_desc"


class Task(BaseModel):
    id: UUID = Field(default_factory=uuid4)
    title: str = Field(..., min_length=1, max_length=200)
    description: str = Field(default="")
    status: TaskStatus = Field(default=TaskStatus.PENDING)
    priority: TaskPriority = Field(default=TaskPriority.MEDIUM)
    tags: List[str] = Field(default_factory=list)
    created_by: str = Field(default="system")
    assigned_to: Optional[str] = Field(default=None)
    created_at: datetime = Field(default_factory=datetime.utcnow)
    updated_at: datetime = Field(default_factory=datetime.utcnow)
    due_date: Optional[datetime] = Field(default=None)
    completed_at: Optional[datetime] = Field(default=None)
    
    class Config:
        json_encoders = {
            datetime: lambda v: v.isoformat(),
            UUID: lambda v: str(v)
        }
    
    def update(self, **kwargs):
        """Update task fields and set updated_at timestamp"""
        for key, value in kwargs.items():
            if hasattr(self, key) and value is not None:
                setattr(self, key, value)
        
        # Update timestamps
        self.updated_at = datetime.utcnow()
        
        # Set completed_at if status changes to completed
        if self.status == TaskStatus.COMPLETED and self.completed_at is None:
            self.completed_at = datetime.utcnow()
        
        return self
    
    def matches_filters(
        self,
        status: Optional[TaskStatus] = None,
        assigned_to: Optional[str] = None,
        tags: Optional[str] = None
    ) -> bool:
        """Check if task matches the given filters"""
        if status and self.status != status:
            return False
        
        if assigned_to and self.assigned_to != assigned_to:
            return False
        
        if tags:
            required_tags = [tag.strip() for tag in tags.split(',')]
            if not all(tag in self.tags for tag in required_tags):
                return False
        
        return True


class CreateTaskRequest(BaseModel):
    title: str = Field(..., min_length=1, max_length=200)
    description: Optional[str] = Field(default="")
    priority: Optional[TaskPriority] = Field(default=TaskPriority.MEDIUM)
    tags: Optional[List[str]] = Field(default_factory=list)
    assigned_to: Optional[str] = Field(default=None)
    due_date: Optional[datetime] = Field(default=None)


class UpdateTaskRequest(BaseModel):
    title: Optional[str] = Field(None, min_length=1, max_length=200)
    description: Optional[str] = Field(None)
    status: Optional[TaskStatus] = Field(None)
    priority: Optional[TaskPriority] = Field(None)
    tags: Optional[List[str]] = Field(None)
    assigned_to: Optional[str] = Field(None)
    due_date: Optional[datetime] = Field(None)


class UpdateTaskStatusRequest(BaseModel):
    status: TaskStatus


class ListTasksQuery(BaseModel):
    page_size: int = Field(default=20, ge=1, le=100)
    page_token: Optional[str] = Field(default=None)
    status: Optional[TaskStatus] = Field(default=None)
    assigned_to: Optional[str] = Field(default=None)
    tags: Optional[str] = Field(default=None)
    sort_order: Optional[SortOrder] = Field(default=None)


class ListTasksResponse(BaseModel):
    tasks: List[Task]
    next_page_token: Optional[str] = Field(default=None)
    total_count: int


class ErrorResponse(BaseModel):
    error: dict = Field(...)
    
    @classmethod
    def create(cls, code: str, message: str):
        return cls(error={"code": code, "message": message})