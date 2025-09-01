<?php
declare(strict_types=1);

namespace TaskAPI\Models;

use JsonSerializable;
use Ramsey\Uuid\Uuid;

class Task implements JsonSerializable
{
    public const STATUS_PENDING = 'pending';
    public const STATUS_IN_PROGRESS = 'in_progress';
    public const STATUS_COMPLETED = 'completed';
    public const STATUS_CANCELLED = 'cancelled';
    public const STATUS_ON_HOLD = 'on_hold';
    
    public const PRIORITY_LOW = 'low';
    public const PRIORITY_MEDIUM = 'medium';
    public const PRIORITY_HIGH = 'high';
    public const PRIORITY_CRITICAL = 'critical';
    
    private string $id;
    private string $title;
    private string $description;
    private string $status;
    private string $priority;
    private array $tags;
    private string $createdBy;
    private ?string $assignedTo;
    private string $createdAt;
    private string $updatedAt;
    private ?string $dueDate;
    private ?string $completedAt;
    
    public function __construct(
        string $title,
        string $description = '',
        string $status = self::STATUS_PENDING,
        string $priority = self::PRIORITY_MEDIUM,
        array $tags = [],
        string $createdBy = 'system',
        ?string $assignedTo = null,
        ?string $dueDate = null
    ) {
        $this->id = Uuid::uuid4()->toString();
        $this->title = $title;
        $this->description = $description;
        $this->status = $status;
        $this->priority = $priority;
        $this->tags = $tags;
        $this->createdBy = $createdBy;
        $this->assignedTo = $assignedTo;
        $this->createdAt = date('c');
        $this->updatedAt = date('c');
        $this->dueDate = $dueDate;
        $this->completedAt = null;
    }
    
    public static function fromArray(array $data): self
    {
        $task = new self(
            $data['title'],
            $data['description'] ?? '',
            $data['status'] ?? self::STATUS_PENDING,
            $data['priority'] ?? self::PRIORITY_MEDIUM,
            $data['tags'] ?? [],
            $data['created_by'] ?? 'system',
            $data['assigned_to'] ?? null,
            $data['due_date'] ?? null
        );
        
        if (isset($data['id'])) {
            $task->id = $data['id'];
        }
        
        return $task;
    }
    
    public function getId(): string
    {
        return $this->id;
    }
    
    public function getTitle(): string
    {
        return $this->title;
    }
    
    public function setTitle(string $title): void
    {
        $this->title = $title;
        $this->updatedAt = date('c');
    }
    
    public function getDescription(): string
    {
        return $this->description;
    }
    
    public function setDescription(string $description): void
    {
        $this->description = $description;
        $this->updatedAt = date('c');
    }
    
    public function getStatus(): string
    {
        return $this->status;
    }
    
    public function setStatus(string $status): void
    {
        $this->status = $status;
        if ($status === self::STATUS_COMPLETED && $this->completedAt === null) {
            $this->completedAt = date('c');
        }
        $this->updatedAt = date('c');
    }
    
    public function getPriority(): string
    {
        return $this->priority;
    }
    
    public function setPriority(string $priority): void
    {
        $this->priority = $priority;
        $this->updatedAt = date('c');
    }
    
    public function getTags(): array
    {
        return $this->tags;
    }
    
    public function setTags(array $tags): void
    {
        $this->tags = $tags;
        $this->updatedAt = date('c');
    }
    
    public function getAssignedTo(): ?string
    {
        return $this->assignedTo;
    }
    
    public function setAssignedTo(?string $assignedTo): void
    {
        $this->assignedTo = $assignedTo;
        $this->updatedAt = date('c');
    }
    
    public function getDueDate(): ?string
    {
        return $this->dueDate;
    }
    
    public function setDueDate(?string $dueDate): void
    {
        $this->dueDate = $dueDate;
        $this->updatedAt = date('c');
    }
    
    public function getCreatedAt(): string
    {
        return $this->createdAt;
    }
    
    public function getUpdatedAt(): string
    {
        return $this->updatedAt;
    }
    
    public function getCompletedAt(): ?string
    {
        return $this->completedAt;
    }
    
    public function isValid(): bool
    {
        if (empty($this->title) || strlen($this->title) > 200) {
            return false;
        }
        
        if (!in_array($this->status, $this->getValidStatuses())) {
            return false;
        }
        
        if (!in_array($this->priority, $this->getValidPriorities())) {
            return false;
        }
        
        return true;
    }
    
    public function getValidationErrors(): array
    {
        $errors = [];
        
        if (empty($this->title)) {
            $errors[] = 'Title is required';
        } elseif (strlen($this->title) > 200) {
            $errors[] = 'Title must be 200 characters or less';
        }
        
        if (!in_array($this->status, $this->getValidStatuses())) {
            $errors[] = "Invalid status: {$this->status}";
        }
        
        if (!in_array($this->priority, $this->getValidPriorities())) {
            $errors[] = "Invalid priority: {$this->priority}";
        }
        
        return $errors;
    }
    
    public function update(array $data): void
    {
        if (isset($data['title'])) {
            $this->setTitle($data['title']);
        }
        
        if (isset($data['description'])) {
            $this->setDescription($data['description']);
        }
        
        if (isset($data['status'])) {
            $this->setStatus($data['status']);
        }
        
        if (isset($data['priority'])) {
            $this->setPriority($data['priority']);
        }
        
        if (isset($data['tags'])) {
            $this->setTags($data['tags']);
        }
        
        if (array_key_exists('assigned_to', $data)) {
            $this->setAssignedTo($data['assigned_to']);
        }
        
        if (array_key_exists('due_date', $data)) {
            $this->setDueDate($data['due_date']);
        }
    }
    
    public function matchesFilters(?string $status = null, ?string $assignedTo = null, ?string $tags = null): bool
    {
        if ($status !== null && $this->status !== $status) {
            return false;
        }
        
        if ($assignedTo !== null && $this->assignedTo !== $assignedTo) {
            return false;
        }
        
        if ($tags !== null) {
            $requiredTags = array_map('trim', explode(',', $tags));
            foreach ($requiredTags as $tag) {
                if (!in_array($tag, $this->tags)) {
                    return false;
                }
            }
        }
        
        return true;
    }
    
    public function getPriorityValue(): int
    {
        return match($this->priority) {
            self::PRIORITY_LOW => 1,
            self::PRIORITY_MEDIUM => 2,
            self::PRIORITY_HIGH => 3,
            self::PRIORITY_CRITICAL => 4,
            default => 2
        };
    }
    
    public static function getValidStatuses(): array
    {
        return [
            self::STATUS_PENDING,
            self::STATUS_IN_PROGRESS,
            self::STATUS_COMPLETED,
            self::STATUS_CANCELLED,
            self::STATUS_ON_HOLD
        ];
    }
    
    public static function getValidPriorities(): array
    {
        return [
            self::PRIORITY_LOW,
            self::PRIORITY_MEDIUM,
            self::PRIORITY_HIGH,
            self::PRIORITY_CRITICAL
        ];
    }
    
    public function jsonSerialize(): array
    {
        $data = [
            'id' => $this->id,
            'title' => $this->title,
            'description' => $this->description,
            'status' => $this->status,
            'priority' => $this->priority,
            'tags' => $this->tags,
            'created_by' => $this->createdBy,
            'created_at' => $this->createdAt,
            'updated_at' => $this->updatedAt
        ];
        
        if ($this->assignedTo !== null) {
            $data['assigned_to'] = $this->assignedTo;
        }
        
        if ($this->dueDate !== null) {
            $data['due_date'] = $this->dueDate;
        }
        
        if ($this->completedAt !== null) {
            $data['completed_at'] = $this->completedAt;
        }
        
        return $data;
    }
}