use crate::models::{Task, TaskStatus, TaskPriority};
use std::collections::HashMap;
use std::sync::RwLock;

pub struct AppState {
    pub tasks: RwLock<HashMap<String, Task>>,
}

impl AppState {
    pub fn new() -> Self {
        let mut tasks = HashMap::new();
        
        // Initialize with sample data
        let mut task1 = Task::new("Complete project documentation".to_string());
        task1.description = Some("Write comprehensive documentation for the REST API".to_string());
        task1.status = TaskStatus::InProgress;
        task1.priority = TaskPriority::High;
        task1.tags = vec!["documentation".to_string(), "api".to_string()];
        task1.assigned_to = Some("dev-team".to_string());
        tasks.insert(task1.id.clone(), task1);
        
        let mut task2 = Task::new("Review pull requests".to_string());
        task2.description = Some("Review and approve pending pull requests".to_string());
        task2.priority = TaskPriority::Medium;
        task2.tags = vec!["review".to_string(), "code".to_string()];
        task2.assigned_to = Some("senior-dev".to_string());
        tasks.insert(task2.id.clone(), task2);
        
        let mut task3 = Task::new("Deploy to production".to_string());
        task3.description = Some("Deploy the latest version to production environment".to_string());
        task3.priority = TaskPriority::Critical;
        task3.tags = vec!["deployment".to_string(), "production".to_string()];
        task3.assigned_to = Some("devops".to_string());
        tasks.insert(task3.id.clone(), task3);
        
        Self {
            tasks: RwLock::new(tasks),
        }
    }
}