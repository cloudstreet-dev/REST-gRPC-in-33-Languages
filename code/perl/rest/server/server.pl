#!/usr/bin/env perl
use strict;
use warnings;
use lib 'lib';
use Mojolicious::Lite;
use Mojo::JSON qw(decode_json encode_json);
use TaskRepository;

# Initialize repository
my $repository = TaskRepository->new();

# Enable CORS
hook before_dispatch => sub {
    my $c = shift;
    $c->res->headers->header('Access-Control-Allow-Origin' => '*');
    $c->res->headers->header('Access-Control-Allow-Methods' => 'GET, POST, PUT, PATCH, DELETE, OPTIONS');
    $c->res->headers->header('Access-Control-Allow-Headers' => 'Content-Type');
};

# Handle OPTIONS requests
options '*' => sub {
    my $c = shift;
    $c->render(text => '', status => 204);
};

# GET /health - Health check
get '/health' => sub {
    my $c = shift;
    
    $c->render(json => {
        status => 'healthy',
        service => 'perl-task-api',
        task_count => $repository->count(),
    });
};

# GET /api/tasks - List all tasks
get '/api/tasks' => sub {
    my $c = shift;
    
    my %filters = (
        status => $c->param('status'),
        assigned_to => $c->param('assigned_to'),
        page_size => $c->param('page_size') || 20,
        page_token => $c->param('page_token'),
        sort_by => $c->param('sort_by') || 'created_at',
        sort_order => $c->param('sort_order') || 'desc',
    );
    
    # Parse tags
    if (my $tags_param = $c->param('tags')) {
        $filters{tags} = [split(',', $tags_param)];
    }
    
    my $result = $repository->list_tasks(%filters);
    
    # Convert tasks to hashes
    my @task_hashes = map { $_->to_hash() } @{$result->{tasks}};
    $result->{tasks} = \@task_hashes;
    
    $c->render(json => $result);
};

# GET /api/tasks/:id - Get a specific task
get '/api/tasks/:id' => sub {
    my $c = shift;
    my $id = $c->param('id');
    
    my $task = $repository->get_task($id);
    
    if ($task) {
        $c->render(json => $task->to_hash());
    } else {
        $c->render(json => { error => 'Task not found' }, status => 404);
    }
};

# POST /api/tasks - Create a new task
post '/api/tasks' => sub {
    my $c = shift;
    
    my $data = $c->req->json;
    
    unless ($data && $data->{title}) {
        return $c->render(json => { error => 'Title is required' }, status => 400);
    }
    
    eval {
        my $task = $repository->create_task($data);
        $c->render(json => $task->to_hash(), status => 201);
    };
    if ($@) {
        $c->render(json => { error => $@ }, status => 400);
    }
};

# PUT /api/tasks/:id - Update a task
put '/api/tasks/:id' => sub {
    my $c = shift;
    my $id = $c->param('id');
    
    my $data = $c->req->json;
    
    my $task = $repository->update_task($id, $data);
    
    if ($task) {
        $c->render(json => $task->to_hash());
    } else {
        $c->render(json => { error => 'Task not found' }, status => 404);
    }
};

# PATCH /api/tasks/:id/status - Update task status
patch '/api/tasks/:id/status' => sub {
    my $c = shift;
    my $id = $c->param('id');
    my $status = $c->param('status');
    
    unless ($status) {
        return $c->render(json => { error => 'Status is required' }, status => 400);
    }
    
    my $task = $repository->update_task_status($id, $status);
    
    if ($task) {
        $c->render(json => $task->to_hash());
    } else {
        $c->render(json => { error => 'Task not found' }, status => 404);
    }
};

# DELETE /api/tasks/:id - Delete a task
del '/api/tasks/:id' => sub {
    my $c = shift;
    my $id = $c->param('id');
    
    if ($repository->delete_task($id)) {
        $c->render(text => '', status => 204);
    } else {
        $c->render(json => { error => 'Task not found' }, status => 404);
    }
};

# Print startup banner
print <<'BANNER';
╔════════════════════════════════════════════════╗
║         Perl Task Management REST API          ║
║           Built with Mojolicious               ║
╚════════════════════════════════════════════════╝

BANNER

my $port = $ENV{PORT} || 8080;

print "[INFO] Perl Task REST Server starting on port $port\n";
print "[INFO] Visit http://localhost:$port/api/tasks\n\n";

print "Available endpoints:\n";
print "  GET    /api/tasks          - List all tasks\n";
print "  GET    /api/tasks/{id}     - Get a specific task\n";
print "  POST   /api/tasks          - Create a new task\n";
print "  PUT    /api/tasks/{id}     - Update a task\n";
print "  PATCH  /api/tasks/{id}/status - Update task status\n";
print "  DELETE /api/tasks/{id}     - Delete a task\n";
print "  GET    /health             - Health check\n\n";

print "Sample requests:\n";
print "  curl http://localhost:$port/api/tasks\n";
print "  curl -X POST http://localhost:$port/api/tasks \\\n";
print "    -H \"Content-Type: application/json\" \\\n";
print "    -d '{\"title\":\"New Task\",\"priority\":\"high\"}'\n\n";

print "[INFO] Press Ctrl+C to stop the server\n\n";

# Start the application
app->start('daemon', '-l', "http://*:$port");