from contextlib import asynccontextmanager
from datetime import datetime
from typing import Optional

from fastapi import FastAPI, HTTPException, Query, Response, status
from fastapi.middleware.cors import CORSMiddleware
from fastapi.responses import JSONResponse
import uvicorn

from models import (
    Task, TaskStatus, SortOrder,
    CreateTaskRequest, UpdateTaskRequest, UpdateTaskStatusRequest,
    ListTasksResponse, ErrorResponse
)
from services import TaskService


# Initialize service
task_service = TaskService()


@asynccontextmanager
async def lifespan(app: FastAPI):
    # Startup
    print("Starting Task API server...")
    yield
    # Shutdown
    print("Shutting down Task API server...")


# Create FastAPI app
app = FastAPI(
    title="Task Management API",
    version="1.0.0",
    description="REST API for task management built with FastAPI",
    lifespan=lifespan
)

# Configure CORS
app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)


# Exception handlers
@app.exception_handler(ValueError)
async def value_error_handler(request, exc):
    if "not found" in str(exc).lower():
        return JSONResponse(
            status_code=status.HTTP_404_NOT_FOUND,
            content={"error": {"code": "NOT_FOUND", "message": str(exc)}}
        )
    return JSONResponse(
        status_code=status.HTTP_400_BAD_REQUEST,
        content={"error": {"code": "VALIDATION_ERROR", "message": str(exc)}}
    )


@app.exception_handler(Exception)
async def general_exception_handler(request, exc):
    return JSONResponse(
        status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
        content={"error": {"code": "INTERNAL_ERROR", "message": str(exc)}}
    )


# Health check endpoint
@app.get("/health")
async def health_check():
    return {
        "status": "healthy",
        "timestamp": datetime.utcnow().isoformat(),
        "service": "task-api-python",
        "version": "1.0.0"
    }


# List tasks
@app.get("/api/v1/tasks", response_model=ListTasksResponse)
async def list_tasks(
    page_size: int = Query(default=20, ge=1, le=100),
    page_token: Optional[str] = Query(default=None),
    status: Optional[TaskStatus] = Query(default=None),
    assigned_to: Optional[str] = Query(default=None),
    tags: Optional[str] = Query(default=None),
    sort_order: Optional[SortOrder] = Query(default=None)
):
    from models import ListTasksQuery
    
    query = ListTasksQuery(
        page_size=page_size,
        page_token=page_token,
        status=status,
        assigned_to=assigned_to,
        tags=tags,
        sort_order=sort_order
    )
    
    return await task_service.list_tasks(query)


# Get task by ID
@app.get("/api/v1/tasks/{task_id}", response_model=Task)
async def get_task(task_id: str):
    try:
        return await task_service.get_task(task_id)
    except ValueError as e:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail={"error": {"code": "NOT_FOUND", "message": str(e)}}
        )


# Create task
@app.post("/api/v1/tasks", response_model=Task, status_code=status.HTTP_201_CREATED)
async def create_task(request: CreateTaskRequest, response: Response):
    task = await task_service.create_task(request)
    response.headers["Location"] = f"/api/v1/tasks/{task.id}"
    return task


# Update task
@app.put("/api/v1/tasks/{task_id}", response_model=Task)
async def update_task(task_id: str, request: UpdateTaskRequest):
    try:
        return await task_service.update_task(task_id, request)
    except ValueError as e:
        if "not found" in str(e).lower():
            raise HTTPException(
                status_code=status.HTTP_404_NOT_FOUND,
                detail={"error": {"code": "NOT_FOUND", "message": str(e)}}
            )
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail={"error": {"code": "VALIDATION_ERROR", "message": str(e)}}
        )


# Update task status
@app.patch("/api/v1/tasks/{task_id}/status", response_model=Task)
async def update_task_status(task_id: str, request: UpdateTaskStatusRequest):
    try:
        return await task_service.update_task_status(task_id, request.status)
    except ValueError as e:
        if "not found" in str(e).lower():
            raise HTTPException(
                status_code=status.HTTP_404_NOT_FOUND,
                detail={"error": {"code": "NOT_FOUND", "message": str(e)}}
            )
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail={"error": {"code": "VALIDATION_ERROR", "message": str(e)}}
        )


# Delete task
@app.delete("/api/v1/tasks/{task_id}", status_code=status.HTTP_204_NO_CONTENT)
async def delete_task(task_id: str):
    try:
        await task_service.delete_task(task_id)
    except ValueError as e:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail={"error": {"code": "NOT_FOUND", "message": str(e)}}
        )


if __name__ == "__main__":
    uvicorn.run(
        "main:app",
        host="0.0.0.0",
        port=8000,
        reload=True,
        log_level="info"
    )