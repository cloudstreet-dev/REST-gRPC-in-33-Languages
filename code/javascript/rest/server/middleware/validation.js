export function validateTask(req, res, next) {
  const { title, priority } = req.body;
  
  const errors = [];
  
  if (!title || title.trim().length === 0) {
    errors.push('Title is required');
  }
  
  if (title && title.length > 200) {
    errors.push('Title must be 200 characters or less');
  }
  
  const validPriorities = ['low', 'medium', 'high', 'critical'];
  if (priority && !validPriorities.includes(priority)) {
    errors.push(`Priority must be one of: ${validPriorities.join(', ')}`);
  }
  
  if (errors.length > 0) {
    return res.status(400).json({ 
      error: 'Validation failed',
      details: errors 
    });
  }
  
  next();
}

export function validateTaskUpdate(req, res, next) {
  const { title, status, priority } = req.body;
  
  const errors = [];
  
  if (title !== undefined) {
    if (title.trim().length === 0) {
      errors.push('Title cannot be empty');
    }
    if (title.length > 200) {
      errors.push('Title must be 200 characters or less');
    }
  }
  
  const validStatuses = ['pending', 'in_progress', 'completed', 'cancelled', 'on_hold'];
  if (status && !validStatuses.includes(status)) {
    errors.push(`Status must be one of: ${validStatuses.join(', ')}`);
  }
  
  const validPriorities = ['low', 'medium', 'high', 'critical'];
  if (priority && !validPriorities.includes(priority)) {
    errors.push(`Priority must be one of: ${validPriorities.join(', ')}`);
  }
  
  if (errors.length > 0) {
    return res.status(400).json({ 
      error: 'Validation failed',
      details: errors 
    });
  }
  
  next();
}