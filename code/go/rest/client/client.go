package client

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"net/url"
	"strconv"
	"strings"
	"time"

	"github.com/cloudstreet-dev/REST-gRPC-in-33-Languages/code/go/internal/models"
)

// TaskAPIClient is a client for the Task Management REST API
type TaskAPIClient struct {
	baseURL    string
	httpClient *http.Client
	headers    map[string]string
}

// NewTaskAPIClient creates a new REST API client
func NewTaskAPIClient(baseURL string) *TaskAPIClient {
	if baseURL == "" {
		baseURL = "http://localhost:8080/api/v1"
	}

	return &TaskAPIClient{
		baseURL: baseURL,
		httpClient: &http.Client{
			Timeout: 30 * time.Second,
		},
		headers: map[string]string{
			"Content-Type": "application/json",
			"Accept":       "application/json",
		},
	}
}

// SetAuthToken sets the authorization token
func (c *TaskAPIClient) SetAuthToken(token string) {
	c.headers["Authorization"] = "Bearer " + token
}

// SetAPIKey sets the API key
func (c *TaskAPIClient) SetAPIKey(apiKey string) {
	c.headers["X-API-Key"] = apiKey
}

// doRequest performs an HTTP request
func (c *TaskAPIClient) doRequest(method, path string, body interface{}) (*http.Response, error) {
	var bodyReader io.Reader
	if body != nil {
		jsonBody, err := json.Marshal(body)
		if err != nil {
			return nil, fmt.Errorf("failed to marshal request body: %w", err)
		}
		bodyReader = bytes.NewReader(jsonBody)
	}

	req, err := http.NewRequest(method, c.baseURL+path, bodyReader)
	if err != nil {
		return nil, fmt.Errorf("failed to create request: %w", err)
	}

	// Set headers
	for key, value := range c.headers {
		req.Header.Set(key, value)
	}

	resp, err := c.httpClient.Do(req)
	if err != nil {
		return nil, fmt.Errorf("request failed: %w", err)
	}

	return resp, nil
}

// parseResponse parses the HTTP response
func (c *TaskAPIClient) parseResponse(resp *http.Response, target interface{}) error {
	defer resp.Body.Close()

	if resp.StatusCode >= 400 {
		var errResp models.ErrorResponse
		if err := json.NewDecoder(resp.Body).Decode(&errResp); err != nil {
			return fmt.Errorf("HTTP %d: failed to parse error response", resp.StatusCode)
		}
		return fmt.Errorf("HTTP %d: %s - %s", resp.StatusCode, errResp.Error.Code, errResp.Error.Message)
	}

	if target != nil && resp.StatusCode != http.StatusNoContent {
		if err := json.NewDecoder(resp.Body).Decode(target); err != nil {
			return fmt.Errorf("failed to parse response: %w", err)
		}
	}

	return nil
}

// ListTasks retrieves a list of tasks
func (c *TaskAPIClient) ListTasks(query models.ListTasksQuery) (*models.ListTasksResponse, error) {
	params := url.Values{}
	
	if query.PageSize > 0 {
		params.Set("page_size", strconv.Itoa(query.PageSize))
	}
	if query.PageToken != "" {
		params.Set("page_token", query.PageToken)
	}
	if query.Status != "" {
		params.Set("status", string(query.Status))
	}
	if query.AssignedTo != "" {
		params.Set("assigned_to", query.AssignedTo)
	}
	if query.Tags != "" {
		params.Set("tags", query.Tags)
	}
	if query.SortOrder != "" {
		params.Set("sort_order", string(query.SortOrder))
	}

	path := "/tasks"
	if len(params) > 0 {
		path += "?" + params.Encode()
	}

	resp, err := c.doRequest("GET", path, nil)
	if err != nil {
		return nil, err
	}

	var result models.ListTasksResponse
	if err := c.parseResponse(resp, &result); err != nil {
		return nil, err
	}

	return &result, nil
}

// GetTask retrieves a single task by ID
func (c *TaskAPIClient) GetTask(id string) (*models.Task, error) {
	resp, err := c.doRequest("GET", "/tasks/"+id, nil)
	if err != nil {
		return nil, err
	}

	var task models.Task
	if err := c.parseResponse(resp, &task); err != nil {
		return nil, err
	}

	return &task, nil
}

// CreateTask creates a new task
func (c *TaskAPIClient) CreateTask(req models.CreateTaskRequest) (*models.Task, error) {
	resp, err := c.doRequest("POST", "/tasks", req)
	if err != nil {
		return nil, err
	}

	var task models.Task
	if err := c.parseResponse(resp, &task); err != nil {
		return nil, err
	}

	return &task, nil
}

// UpdateTask updates an existing task
func (c *TaskAPIClient) UpdateTask(id string, req models.UpdateTaskRequest) (*models.Task, error) {
	resp, err := c.doRequest("PUT", "/tasks/"+id, req)
	if err != nil {
		return nil, err
	}

	var task models.Task
	if err := c.parseResponse(resp, &task); err != nil {
		return nil, err
	}

	return &task, nil
}

// UpdateTaskStatus updates the status of a task
func (c *TaskAPIClient) UpdateTaskStatus(id string, status models.TaskStatus) (*models.Task, error) {
	req := models.UpdateTaskStatusRequest{
		Status: status,
	}

	resp, err := c.doRequest("PATCH", "/tasks/"+id+"/status", req)
	if err != nil {
		return nil, err
	}

	var task models.Task
	if err := c.parseResponse(resp, &task); err != nil {
		return nil, err
	}

	return &task, nil
}

// DeleteTask deletes a task
func (c *TaskAPIClient) DeleteTask(id string) error {
	resp, err := c.doRequest("DELETE", "/tasks/"+id, nil)
	if err != nil {
		return err
	}

	return c.parseResponse(resp, nil)
}

// PrintTaskList prints a formatted list of tasks
func PrintTaskList(tasks []models.Task) {
	for _, task := range tasks {
		status := strings.ToUpper(string(task.Status))
		priority := strings.ToUpper(string(task.Priority))
		
		fmt.Printf("[%s] %s (Priority: %s)\n", status, task.Title, priority)
		if task.Description != "" {
			fmt.Printf("  Description: %s\n", task.Description)
		}
		if len(task.Tags) > 0 {
			fmt.Printf("  Tags: %s\n", strings.Join(task.Tags, ", "))
		}
		if task.AssignedTo != nil {
			fmt.Printf("  Assigned to: %s\n", *task.AssignedTo)
		}
		if task.DueDate != nil {
			fmt.Printf("  Due: %s\n", task.DueDate.Format("2006-01-02"))
		}
		fmt.Println()
	}
}