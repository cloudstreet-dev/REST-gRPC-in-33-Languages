package com.taskapi.model;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Size;
import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class Task {
    private String id;
    
    @NotBlank(message = "Title is required")
    @Size(max = 200, message = "Title must be 200 characters or less")
    private String title;
    
    private String description;
    
    @Builder.Default
    private TaskStatus status = TaskStatus.PENDING;
    
    @Builder.Default
    private TaskPriority priority = TaskPriority.MEDIUM;
    
    private List<String> tags;
    
    private String assignedTo;
    
    private LocalDateTime dueDate;
    
    @Builder.Default
    private LocalDateTime createdAt = LocalDateTime.now();
    
    @Builder.Default
    private LocalDateTime updatedAt = LocalDateTime.now();
    
    private String createdBy;
    
    private String updatedBy;
    
    public static Task create() {
        return Task.builder()
                .id(UUID.randomUUID().toString())
                .createdAt(LocalDateTime.now())
                .updatedAt(LocalDateTime.now())
                .build();
    }
}