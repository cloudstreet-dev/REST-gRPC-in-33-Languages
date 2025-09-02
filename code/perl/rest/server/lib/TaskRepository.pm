package TaskRepository;
use strict;
use warnings;
use Task;
use Thread::Semaphore;

sub new {
    my $class = shift;
    
    my $self = {
        tasks => {},
        semaphore => Thread::Semaphore->new(1),
    };
    
    bless $self, $class;
    $self->_load_sample_data();
    return $self;
}

sub _load_sample_data {
    my $self = shift;
    
    # Sample task 1
    my $task1 = Task->new(
        title       => 'Implement Perl REST API',
        description => 'Build a REST API server using Mojolicious framework',
        status      => Task::STATUS_IN_PROGRESS,
        priority    => Task::PRIORITY_HIGH,
        tags        => ['perl', 'rest', 'api'],
        assigned_to => 'backend-team',
    );
    $self->{tasks}->{$task1->{id}} = $task1;
    
    # Sample task 2
    my $task2 = Task->new(
        title       => 'Add gRPC support',
        description => 'Implement gRPC server and client for Perl',
        status      => Task::STATUS_PENDING,
        priority    => Task::PRIORITY_MEDIUM,
        tags        => ['perl', 'grpc', 'protobuf'],
        assigned_to => 'backend-team',
    );
    $self->{tasks}->{$task2->{id}} = $task2;
    
    # Sample task 3
    my $task3 = Task->new(
        title       => 'Write unit tests',
        description => 'Add comprehensive test coverage using Test::More',
        status      => Task::STATUS_PENDING,
        priority    => Task::PRIORITY_HIGH,
        tags        => ['testing', 'quality'],
        assigned_to => 'qa-team',
    );
    $self->{tasks}->{$task3->{id}} = $task3;
}

sub list_tasks {
    my ($self, %filters) = @_;
    
    $self->{semaphore}->down;
    
    my @tasks = values %{$self->{tasks}};
    
    # Filter by status
    if ($filters{status}) {
        @tasks = grep { $_->{status} eq $filters{status} } @tasks;
    }
    
    # Filter by assigned_to
    if ($filters{assigned_to}) {
        @tasks = grep { $_->{assigned_to} eq $filters{assigned_to} } @tasks;
    }
    
    # Filter by tags
    if ($filters{tags} && ref($filters{tags}) eq 'ARRAY') {
        @tasks = grep {
            my $task = $_;
            my $has_all_tags = 1;
            for my $tag (@{$filters{tags}}) {
                unless (grep { $_ eq $tag } @{$task->{tags}}) {
                    $has_all_tags = 0;
                    last;
                }
            }
            $has_all_tags;
        } @tasks;
    }
    
    # Sort
    my $sort_by = $filters{sort_by} || 'created_at';
    my $sort_order = $filters{sort_order} || 'desc';
    
    if ($sort_by eq 'title') {
        if ($sort_order eq 'asc') {
            @tasks = sort { $a->{title} cmp $b->{title} } @tasks;
        } else {
            @tasks = sort { $b->{title} cmp $a->{title} } @tasks;
        }
    } elsif ($sort_by eq 'updated_at') {
        if ($sort_order eq 'asc') {
            @tasks = sort { $a->{updated_at} cmp $b->{updated_at} } @tasks;
        } else {
            @tasks = sort { $b->{updated_at} cmp $a->{updated_at} } @tasks;
        }
    } else {  # created_at
        if ($sort_order eq 'asc') {
            @tasks = sort { $a->{created_at} cmp $b->{created_at} } @tasks;
        } else {
            @tasks = sort { $b->{created_at} cmp $a->{created_at} } @tasks;
        }
    }
    
    # Pagination
    my $page_size = $filters{page_size} || 20;
    $page_size = 100 if $page_size > 100;
    
    my $offset = $filters{page_token} || 0;
    
    my $total = scalar @tasks;
    my $end = $offset + $page_size;
    $end = $total if $end > $total;
    
    my @page = @tasks[$offset .. $end - 1] if $offset < $total;
    
    $self->{semaphore}->up;
    
    return {
        tasks => \@page,
        total_count => $total,
        page_size => $page_size,
        next_page_token => ($end < $total) ? $end : undef,
    };
}

sub get_task {
    my ($self, $id) = @_;
    
    $self->{semaphore}->down;
    my $task = $self->{tasks}->{$id};
    $self->{semaphore}->up;
    
    return $task;
}

sub create_task {
    my ($self, $data) = @_;
    
    my $task = Task->from_hash($data);
    
    $self->{semaphore}->down;
    $self->{tasks}->{$task->{id}} = $task;
    $self->{semaphore}->up;
    
    return $task;
}

sub update_task {
    my ($self, $id, $updates) = @_;
    
    $self->{semaphore}->down;
    
    my $task = $self->{tasks}->{$id};
    if ($task) {
        $task->update($updates);
    }
    
    $self->{semaphore}->up;
    
    return $task;
}

sub update_task_status {
    my ($self, $id, $status) = @_;
    
    return $self->update_task($id, { status => $status });
}

sub delete_task {
    my ($self, $id) = @_;
    
    $self->{semaphore}->down;
    
    my $exists = exists $self->{tasks}->{$id};
    delete $self->{tasks}->{$id} if $exists;
    
    $self->{semaphore}->up;
    
    return $exists;
}

sub count {
    my $self = shift;
    
    $self->{semaphore}->down;
    my $count = scalar keys %{$self->{tasks}};
    $self->{semaphore}->up;
    
    return $count;
}

1;