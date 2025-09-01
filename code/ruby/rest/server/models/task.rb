require 'json'
require 'time'
require 'securerandom'

module Models
  class Task
    VALID_STATUSES = %w[pending in_progress completed cancelled on_hold].freeze
    VALID_PRIORITIES = %w[low medium high critical].freeze
    
    attr_accessor :id, :title, :description, :status, :priority, :tags,
                  :created_by, :assigned_to, :created_at, :updated_at,
                  :due_date, :completed_at
    
    def initialize(attrs = {})
      @id = attrs[:id] || SecureRandom.uuid
      @title = attrs[:title] || ''
      @description = attrs[:description] || ''
      @status = attrs[:status] || 'pending'
      @priority = attrs[:priority] || 'medium'
      @tags = attrs[:tags] || []
      @created_by = attrs[:created_by] || 'system'
      @assigned_to = attrs[:assigned_to]
      @created_at = attrs[:created_at] || Time.now
      @updated_at = attrs[:updated_at] || Time.now
      @due_date = attrs[:due_date]
      @completed_at = attrs[:completed_at]
    end
    
    def self.from_json(json_str)
      data = JSON.parse(json_str, symbolize_names: true)
      from_hash(data)
    end
    
    def self.from_hash(data)
      task = new(data)
      
      # Parse time fields
      task.created_at = Time.parse(data[:created_at]) if data[:created_at].is_a?(String)
      task.updated_at = Time.parse(data[:updated_at]) if data[:updated_at].is_a?(String)
      task.due_date = Time.parse(data[:due_date]) if data[:due_date].is_a?(String)
      task.completed_at = Time.parse(data[:completed_at]) if data[:completed_at].is_a?(String)
      
      task
    end
    
    def to_h
      {
        id: @id,
        title: @title,
        description: @description,
        status: @status,
        priority: @priority,
        tags: @tags,
        created_by: @created_by,
        assigned_to: @assigned_to,
        created_at: @created_at.iso8601,
        updated_at: @updated_at.iso8601,
        due_date: @due_date&.iso8601,
        completed_at: @completed_at&.iso8601
      }.compact
    end
    
    def to_json(*args)
      to_h.to_json(*args)
    end
    
    def valid?
      return false if @title.nil? || @title.empty?
      return false if @title.length > 200
      return false unless VALID_STATUSES.include?(@status)
      return false unless VALID_PRIORITIES.include?(@priority)
      true
    end
    
    def validation_errors
      errors = []
      errors << "Title is required" if @title.nil? || @title.empty?
      errors << "Title must be 200 characters or less" if @title && @title.length > 200
      errors << "Invalid status: #{@status}" unless VALID_STATUSES.include?(@status)
      errors << "Invalid priority: #{@priority}" unless VALID_PRIORITIES.include?(@priority)
      errors
    end
    
    def update!(attrs)
      @title = attrs[:title] if attrs.key?(:title)
      @description = attrs[:description] if attrs.key?(:description)
      @status = attrs[:status] if attrs.key?(:status)
      @priority = attrs[:priority] if attrs.key?(:priority)
      @tags = attrs[:tags] if attrs.key?(:tags)
      @assigned_to = attrs[:assigned_to] if attrs.key?(:assigned_to)
      @due_date = attrs[:due_date] if attrs.key?(:due_date)
      
      # Update completed_at if status changes to completed
      if @status == 'completed' && @completed_at.nil?
        @completed_at = Time.now
      end
      
      @updated_at = Time.now
      self
    end
    
    def matches_filters?(status: nil, assigned_to: nil, tags: nil)
      return false if status && @status != status
      return false if assigned_to && @assigned_to != assigned_to
      
      if tags && !tags.empty?
        tag_list = tags.is_a?(String) ? tags.split(',').map(&:strip) : tags
        return false unless (tag_list - @tags).empty?
      end
      
      true
    end
    
    def priority_value
      case @priority
      when 'low' then 1
      when 'medium' then 2
      when 'high' then 3
      when 'critical' then 4
      else 2
      end
    end
  end
end