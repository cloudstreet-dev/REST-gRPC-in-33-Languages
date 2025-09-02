#!/usr/bin/env perl
use strict;
use warnings;
use LWP::UserAgent;
use JSON;
use Getopt::Long;
use Data::Dumper;

# Initialize user agent and JSON encoder
my $ua = LWP::UserAgent->new();
my $json = JSON->new->utf8->pretty();

# Get base URL from environment or use default
my $base_url = $ENV{TASK_API_URL} || 'http://localhost:8080';

sub print_task {
    my $task = shift;
    
    print "╔═══════════════════════════════════════════════════════╗\n";
    printf "║ Task ID: %-44s ║\n", $task->{id};
    printf "║ Title: %-46s ║\n", substr($task->{title}, 0, 46);
    
    if ($task->{description}) {
        printf "║ Description: %-40s ║\n", substr($task->{description}, 0, 40);
    }
    
    printf "║ Status: %-45s ║\n", $task->{status};
    printf "║ Priority: %-43s ║\n", $task->{priority};
    
    if ($task->{tags} && @{$task->{tags}}) {
        my $tags_str = join(', ', @{$task->{tags}});
        printf "║ Tags: %-47s ║\n", substr($tags_str, 0, 47);
    }
    
    if ($task->{assigned_to}) {
        printf "║ Assigned To: %-40s ║\n", $task->{assigned_to};
    }
    
    printf "║ Created: %-44s ║\n", $task->{created_at};
    printf "║ Updated: %-44s ║\n", $task->{updated_at};
    print "╚═══════════════════════════════════════════════════════╝\n";
}

sub api_request {
    my ($method, $path, $data) = @_;
    
    my $url = $base_url . $path;
    my $request;
    
    if ($method eq 'GET') {
        $request = HTTP::Request->new(GET => $url);
    } elsif ($method eq 'POST') {
        $request = HTTP::Request->new(POST => $url);
        $request->header('Content-Type' => 'application/json');
        $request->content($json->encode($data)) if $data;
    } elsif ($method eq 'PUT') {
        $request = HTTP::Request->new(PUT => $url);
        $request->header('Content-Type' => 'application/json');
        $request->content($json->encode($data)) if $data;
    } elsif ($method eq 'PATCH') {
        $request = HTTP::Request->new(PATCH => $url);
    } elsif ($method eq 'DELETE') {
        $request = HTTP::Request->new(DELETE => $url);
    }
    
    my $response = $ua->request($request);
    
    if ($response->is_success) {
        if ($response->code == 204) {
            return { success => 1 };
        }
        return $json->decode($response->content) if $response->content;
    } else {
        die "HTTP Error " . $response->code . ": " . $response->message . "\n";
    }
}

sub list_tasks {
    my %params = @_;
    
    my $query = '';
    if (%params) {
        my @parts;
        for my $key (keys %params) {
            push @parts, "$key=" . $params{$key} if defined $params{$key};
        }
        $query = '?' . join('&', @parts) if @parts;
    }
    
    my $result = api_request('GET', '/api/tasks' . $query);
    
    print "Found $result->{total_count} tasks:\n\n";
    for my $task (@{$result->{tasks}}) {
        print_task($task);
        print "\n";
    }
}

sub get_task {
    my $id = shift;
    
    my $task = api_request('GET', "/api/tasks/$id");
    print_task($task);
}

sub create_task {
    my $data = shift;
    
    my $task = api_request('POST', '/api/tasks', $data);
    print "Task created successfully:\n";
    print_task($task);
}

sub update_task {
    my ($id, $data) = @_;
    
    my $task = api_request('PUT', "/api/tasks/$id", $data);
    print "Task updated successfully:\n";
    print_task($task);
}

sub update_task_status {
    my ($id, $status) = @_;
    
    my $task = api_request('PATCH', "/api/tasks/$id/status?status=$status");
    print "Task status updated successfully:\n";
    print_task($task);
}

sub delete_task {
    my $id = shift;
    
    api_request('DELETE', "/api/tasks/$id");
    print "Task $id deleted successfully\n";
}

sub health_check {
    my $health = api_request('GET', '/health');
    
    print "Server health:\n";
    print "  Status: $health->{status}\n";
    print "  Service: $health->{service}\n";
    print "  Task count: $health->{task_count}\n";
}

sub run_demo {
    print <<'BANNER';
╔════════════════════════════════════════════════╗
║          Task Management API Demo              ║
╚════════════════════════════════════════════════╝

BANNER
    
    eval {
        # Step 1: List tasks
        print "1. Listing all tasks...\n";
        my $result = api_request('GET', '/api/tasks');
        print "   Found $result->{total_count} tasks\n";
        for my $task (@{$result->{tasks}}) {
            print "   - $task->{id}: $task->{title}\n";
        }
        print "\n";
        
        # Step 2: Create a task
        print "2. Creating a new task...\n";
        my $new_task = {
            title => 'Demo Task from Perl Client',
            description => 'This task was created using the Perl REST client',
            priority => 'high',
            tags => ['demo', 'perl', 'api-test'],
            assigned_to => 'demo-user',
        };
        
        my $created = api_request('POST', '/api/tasks', $new_task);
        print "   Created task: $created->{id}\n";
        print_task($created);
        print "\n";
        
        my $task_id = $created->{id};
        
        # Step 3: Get the created task
        print "3. Retrieving the created task...\n";
        my $retrieved = api_request('GET', "/api/tasks/$task_id");
        print "   Retrieved task:\n";
        print_task($retrieved);
        print "\n";
        
        # Step 4: Update the task
        print "4. Updating the task...\n";
        my $updates = {
            title => 'Updated Demo Task',
            description => 'This task has been updated via the API',
            status => 'in_progress',
            priority => 'urgent',
        };
        
        my $updated = api_request('PUT', "/api/tasks/$task_id", $updates);
        print "   Updated task:\n";
        print_task($updated);
        print "\n";
        
        # Step 5: Update task status
        print "5. Updating task status to completed...\n";
        my $status_updated = api_request('PATCH', "/api/tasks/$task_id/status?status=completed");
        print "   Task status updated:\n";
        print_task($status_updated);
        print "\n";
        
        # Step 6: List completed tasks
        print "6. Listing completed tasks...\n";
        my $completed = api_request('GET', '/api/tasks?status=completed');
        print "   Found $completed->{total_count} completed tasks\n";
        for my $task (@{$completed->{tasks}}) {
            print "   - $task->{id}: $task->{title} (Status: $task->{status})\n";
        }
        print "\n";
        
        # Step 7: Delete the task
        print "7. Deleting the demo task...\n";
        api_request('DELETE', "/api/tasks/$task_id");
        print "   Task deleted successfully\n\n";
        
        # Step 8: Verify deletion
        print "8. Verifying task deletion...\n";
        eval {
            api_request('GET', "/api/tasks/$task_id");
            print "   Warning: Task still exists!\n";
        };
        if ($@) {
            print "   Task not found (as expected): $@";
        }
    };
    if ($@) {
        print "Error: $@\n";
    }
    
    print <<'BANNER';

╔════════════════════════════════════════════════╗
║              Demo Complete!                    ║
╚════════════════════════════════════════════════╝
BANNER
}

sub print_help {
    print <<'HELP';
╔════════════════════════════════════════════════╗
║         Perl Task Management CLI               ║
╚════════════════════════════════════════════════╝

Usage: perl client.pl <command> [options]

Commands:
  list                    List all tasks
  get <task-id>          Get a specific task
  create                 Create a new task (reads JSON from STDIN)
  update <task-id>       Update a task (reads JSON from STDIN)
  status <task-id> <status>  Update task status
  delete <task-id>       Delete a task
  health                 Check server health
  demo                   Run a demonstration
  help                   Show this help

Options for list:
  --status <status>      Filter by status
  --assigned-to <user>   Filter by assignee
  --tags <tag1,tag2>     Filter by tags

Environment:
  TASK_API_URL           Base URL for the Task API

Examples:
  perl client.pl list
  perl client.pl get 123e4567-e89b-12d3-a456-426614174000
  echo '{"title":"New Task","priority":"high"}' | perl client.pl create
  perl client.pl status 123e4567-e89b-12d3-a456-426614174000 completed
  perl client.pl delete 123e4567-e89b-12d3-a456-426614174000

HELP
}

# Main program
my $command = shift @ARGV || '';

if ($command eq 'help' || !$command) {
    print_help();
    exit 0;
}

eval {
    if ($command eq 'demo') {
        run_demo();
    } elsif ($command eq 'health') {
        health_check();
    } elsif ($command eq 'list') {
        my %params;
        GetOptions(
            'status=s' => \$params{status},
            'assigned-to=s' => \$params{assigned_to},
            'tags=s' => \$params{tags},
        );
        list_tasks(%params);
    } elsif ($command eq 'get') {
        my $id = shift @ARGV or die "Task ID required\n";
        get_task($id);
    } elsif ($command eq 'create') {
        my $input = do { local $/; <STDIN> };
        my $data = $json->decode($input);
        create_task($data);
    } elsif ($command eq 'update') {
        my $id = shift @ARGV or die "Task ID required\n";
        my $input = do { local $/; <STDIN> };
        my $data = $json->decode($input);
        update_task($id, $data);
    } elsif ($command eq 'status') {
        my $id = shift @ARGV or die "Task ID required\n";
        my $status = shift @ARGV or die "Status required\n";
        update_task_status($id, $status);
    } elsif ($command eq 'delete') {
        my $id = shift @ARGV or die "Task ID required\n";
        delete_task($id);
    } else {
        die "Unknown command: $command\n";
    }
};

if ($@) {
    print "Error: $@";
    exit 1;
}