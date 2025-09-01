import asyncio
from typing import Dict, List, Optional, Any
from datetime import datetime
import httpx
import json


class TaskAPIClient:
    """REST API client for Task Management Service"""
    
    def __init__(self, base_url: str = "http://localhost:8000/api/v1"):
        self.base_url = base_url
        self.headers = {
            "Content-Type": "application/json",
            "Accept": "application/json"
        }
        self.client = httpx.AsyncClient(
            base_url=base_url,
            headers=self.headers,
            timeout=30.0
        )
    
    async def __aenter__(self):
        return self
    
    async def __aexit__(self, exc_type, exc_val, exc_tb):
        await self.close()
    
    async def close(self):
        """Close the HTTP client"""
        await self.client.aclose()
    
    def set_auth_token(self, token: str):
        """Set authorization token"""
        self.headers["Authorization"] = f"Bearer {token}"
        self.client.headers.update(self.headers)
    
    def set_api_key(self, api_key: str):
        """Set API key"""
        self.headers["X-API-Key"] = api_key
        self.client.headers.update(self.headers)
    
    async def list_tasks(
        self,
        page_size: int = 20,
        page_token: Optional[str] = None,
        status: Optional[str] = None,
        assigned_to: Optional[str] = None,
        tags: Optional[str] = None,
        sort_order: Optional[str] = None
    ) -> Dict[str, Any]:
        """List tasks with optional filters"""
        params = {
            "page_size": page_size
        }
        
        if page_token:
            params["page_token"] = page_token
        if status:
            params["status"] = status
        if assigned_to:
            params["assigned_to"] = assigned_to
        if tags:
            params["tags"] = tags
        if sort_order:
            params["sort_order"] = sort_order
        
        response = await self.client.get("/tasks", params=params)
        response.raise_for_status()
        return response.json()
    
    async def get_task(self, task_id: str) -> Dict[str, Any]:
        """Get a single task by ID"""
        response = await self.client.get(f"/tasks/{task_id}")
        
        if response.status_code == 404:
            raise TaskNotFoundError(f"Task with ID {task_id} not found")
        
        response.raise_for_status()
        return response.json()
    
    async def create_task(
        self,
        title: str,
        description: Optional[str] = None,
        priority: Optional[str] = None,
        tags: Optional[List[str]] = None,
        assigned_to: Optional[str] = None,
        due_date: Optional[datetime] = None
    ) -> Dict[str, Any]:
        """Create a new task"""
        data = {
            "title": title
        }
        
        if description is not None:
            data["description"] = description
        if priority:
            data["priority"] = priority
        if tags:
            data["tags"] = tags
        if assigned_to:
            data["assigned_to"] = assigned_to
        if due_date:
            data["due_date"] = due_date.isoformat() if isinstance(due_date, datetime) else due_date
        
        response = await self.client.post("/tasks", json=data)
        
        if response.status_code == 400:
            error = response.json()
            raise ValidationError(error.get("error", {}).get("message", "Validation failed"))
        
        response.raise_for_status()
        return response.json()
    
    async def update_task(
        self,
        task_id: str,
        title: Optional[str] = None,
        description: Optional[str] = None,
        status: Optional[str] = None,
        priority: Optional[str] = None,
        tags: Optional[List[str]] = None,
        assigned_to: Optional[str] = None,
        due_date: Optional[datetime] = None
    ) -> Dict[str, Any]:
        """Update an existing task"""
        data = {}
        
        if title is not None:
            data["title"] = title
        if description is not None:
            data["description"] = description
        if status is not None:
            data["status"] = status
        if priority is not None:
            data["priority"] = priority
        if tags is not None:
            data["tags"] = tags
        if assigned_to is not None:
            data["assigned_to"] = assigned_to
        if due_date is not None:
            data["due_date"] = due_date.isoformat() if isinstance(due_date, datetime) else due_date
        
        response = await self.client.put(f"/tasks/{task_id}", json=data)
        
        if response.status_code == 404:
            raise TaskNotFoundError(f"Task with ID {task_id} not found")
        if response.status_code == 400:
            error = response.json()
            raise ValidationError(error.get("error", {}).get("message", "Validation failed"))
        
        response.raise_for_status()
        return response.json()
    
    async def update_task_status(self, task_id: str, status: str) -> Dict[str, Any]:
        """Update task status"""
        data = {"status": status}
        
        response = await self.client.patch(f"/tasks/{task_id}/status", json=data)
        
        if response.status_code == 404:
            raise TaskNotFoundError(f"Task with ID {task_id} not found")
        if response.status_code == 400:
            error = response.json()
            raise ValidationError(error.get("error", {}).get("message", "Validation failed"))
        
        response.raise_for_status()
        return response.json()
    
    async def delete_task(self, task_id: str) -> bool:
        """Delete a task"""
        response = await self.client.delete(f"/tasks/{task_id}")
        
        if response.status_code == 404:
            raise TaskNotFoundError(f"Task with ID {task_id} not found")
        
        response.raise_for_status()
        return response.status_code == 204


class TaskAPIError(Exception):
    """Base exception for Task API errors"""
    pass


class TaskNotFoundError(TaskAPIError):
    """Task not found error"""
    pass


class ValidationError(TaskAPIError):
    """Validation error"""
    pass


def print_task_list(tasks: List[Dict[str, Any]]):
    """Helper function to print formatted task list"""
    for task in tasks:
        status = task['status'].upper()
        priority = task['priority'].upper()
        title = task['title']
        
        print(f"[{status}] {title} (Priority: {priority})")
        
        if task.get('description'):
            print(f"  Description: {task['description']}")
        
        if task.get('tags'):
            print(f"  Tags: {', '.join(task['tags'])}")
        
        if task.get('assigned_to'):
            print(f"  Assigned to: {task['assigned_to']}")
        
        if task.get('due_date'):
            print(f"  Due: {task['due_date']}")
        
        print()


async def main():
    """Example usage of the Task API client"""
    async with TaskAPIClient() as client:
        try:
            # Create a task
            print("Creating task...")
            task = await client.create_task(
                title="Test Python Client",
                description="Testing the Python REST client",
                priority="high",
                tags=["test", "python"]
            )
            task_id = task['id']
            print(f"Created task: {task_id}")
            
            # List tasks
            print("\nListing tasks...")
            result = await client.list_tasks(page_size=10, sort_order="priority_desc")
            print_task_list(result['tasks'])
            
            # Update task status
            print("Updating task status...")
            updated = await client.update_task_status(task_id, "in_progress")
            print(f"Task status updated to: {updated['status']}")
            
            # Get single task
            print("\nFetching task details...")
            fetched = await client.get_task(task_id)
            print(f"Task: {fetched['title']} - Status: {fetched['status']}")
            
            # Delete task
            print("\nDeleting task...")
            if await client.delete_task(task_id):
                print("Task deleted successfully")
            
        except TaskAPIError as e:
            print(f"Error: {e}")
        except Exception as e:
            print(f"Unexpected error: {e}")


if __name__ == "__main__":
    asyncio.run(main())