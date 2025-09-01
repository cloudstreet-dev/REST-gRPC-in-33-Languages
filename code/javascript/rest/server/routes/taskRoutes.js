import { Router } from 'express';
import { validateTask, validateTaskUpdate } from '../middleware/validation.js';

export function taskRoutes(taskService) {
  const router = Router();
  
  // List tasks
  router.get('/tasks', async (req, res, next) => {
    try {
      const result = await taskService.listTasks(req.query);
      res.json(result);
    } catch (error) {
      next(error);
    }
  });
  
  // Get single task
  router.get('/tasks/:id', async (req, res, next) => {
    try {
      const task = await taskService.getTask(req.params.id);
      res.json(task);
    } catch (error) {
      next(error);
    }
  });
  
  // Create task
  router.post('/tasks', validateTask, async (req, res, next) => {
    try {
      const task = await taskService.createTask(req.body);
      res.status(201)
         .location(`/api/v1/tasks/${task.id}`)
         .json(task);
    } catch (error) {
      next(error);
    }
  });
  
  // Update task
  router.put('/tasks/:id', validateTaskUpdate, async (req, res, next) => {
    try {
      const task = await taskService.updateTask(req.params.id, req.body);
      res.json(task);
    } catch (error) {
      next(error);
    }
  });
  
  // Update task status
  router.patch('/tasks/:id/status', async (req, res, next) => {
    try {
      const { status } = req.body;
      if (!status) {
        return res.status(400).json({ 
          error: 'Status is required' 
        });
      }
      
      const validStatuses = ['pending', 'in_progress', 'completed', 'cancelled', 'on_hold'];
      if (!validStatuses.includes(status)) {
        return res.status(400).json({ 
          error: `Status must be one of: ${validStatuses.join(', ')}`
        });
      }
      
      const task = await taskService.updateTask(req.params.id, { status });
      res.json(task);
    } catch (error) {
      next(error);
    }
  });
  
  // Delete task
  router.delete('/tasks/:id', async (req, res, next) => {
    try {
      await taskService.deleteTask(req.params.id);
      res.status(204).send();
    } catch (error) {
      next(error);
    }
  });
  
  return router;
}