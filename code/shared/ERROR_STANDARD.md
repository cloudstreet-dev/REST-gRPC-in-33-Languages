# Error Response Standard

This document defines the standard error response format that should be used across all REST API implementations in this project.

## Standard Error Response Format

All error responses should follow this structure:

```json
{
  "error": {
    "code": "ERROR_CODE",
    "message": "Human-readable error message",
    "details": {
      // Optional: Additional error details
    }
  }
}
```

## HTTP Status Codes

- **400 Bad Request**: Invalid request data or parameters
- **401 Unauthorized**: Authentication required
- **403 Forbidden**: Authenticated but not authorized
- **404 Not Found**: Resource not found
- **405 Method Not Allowed**: HTTP method not supported
- **409 Conflict**: Resource conflict (e.g., duplicate)
- **422 Unprocessable Entity**: Valid syntax but semantic errors
- **429 Too Many Requests**: Rate limit exceeded
- **500 Internal Server Error**: Server error
- **503 Service Unavailable**: Service temporarily unavailable

## Standard Error Codes

### Client Errors (4xx)
- `INVALID_REQUEST`: General invalid request
- `MISSING_FIELD`: Required field missing
- `INVALID_FIELD`: Field value invalid
- `RESOURCE_NOT_FOUND`: Resource doesn't exist
- `RESOURCE_EXISTS`: Resource already exists
- `INVALID_STATUS`: Invalid status transition
- `INVALID_PRIORITY`: Invalid priority value
- `RATE_LIMIT_EXCEEDED`: Too many requests

### Server Errors (5xx)
- `INTERNAL_ERROR`: General server error
- `DATABASE_ERROR`: Database operation failed
- `SERVICE_UNAVAILABLE`: Service temporarily down

## Examples

### 400 Bad Request
```json
{
  "error": {
    "code": "MISSING_FIELD",
    "message": "Title is required",
    "details": {
      "field": "title"
    }
  }
}
```

### 404 Not Found
```json
{
  "error": {
    "code": "RESOURCE_NOT_FOUND",
    "message": "Task not found",
    "details": {
      "id": "task-123"
    }
  }
}
```

### 422 Unprocessable Entity
```json
{
  "error": {
    "code": "INVALID_STATUS",
    "message": "Cannot transition from 'completed' to 'pending'",
    "details": {
      "current_status": "completed",
      "requested_status": "pending",
      "allowed_transitions": ["archived"]
    }
  }
}
```

### 500 Internal Server Error
```json
{
  "error": {
    "code": "INTERNAL_ERROR",
    "message": "An unexpected error occurred",
    "details": {
      "request_id": "req-456"
    }
  }
}
```

## gRPC Error Mapping

For gRPC implementations, map to standard gRPC status codes:

| HTTP Status | gRPC Status | Error Code |
|------------|-------------|------------|
| 400 | INVALID_ARGUMENT | INVALID_REQUEST |
| 401 | UNAUTHENTICATED | UNAUTHORIZED |
| 403 | PERMISSION_DENIED | FORBIDDEN |
| 404 | NOT_FOUND | RESOURCE_NOT_FOUND |
| 409 | ALREADY_EXISTS | RESOURCE_EXISTS |
| 429 | RESOURCE_EXHAUSTED | RATE_LIMIT_EXCEEDED |
| 500 | INTERNAL | INTERNAL_ERROR |
| 503 | UNAVAILABLE | SERVICE_UNAVAILABLE |

## Implementation Notes

1. **Consistency**: All languages should map their native error types to this format
2. **Localization**: The `message` field should be human-readable but the `code` should remain constant
3. **Details**: The `details` object is optional but should include relevant debugging information
4. **Security**: Never expose sensitive information in error messages (passwords, internal paths, etc.)
5. **Logging**: Log full error details server-side, return sanitized versions to clients

## Validation Rules

### Task Validation
- `title`: Required, 1-200 characters
- `description`: Optional, max 2000 characters
- `status`: Must be one of: pending, in_progress, completed, cancelled, on_hold
- `priority`: Must be one of: low, medium, high, urgent
- `tags`: Array of strings, max 10 tags, each max 50 characters
- `assigned_to`: Optional, valid user identifier

### Request Validation
- Query parameters should be validated and return 400 for invalid values
- Path parameters should return 404 if resource not found
- Body parameters should return 400 for schema violations