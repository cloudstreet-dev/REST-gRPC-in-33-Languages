package Task;
use strict;
use warnings;
use UUID::Tiny ':std';
use Time::Piece;

# Task status constants
use constant {
    STATUS_PENDING     => 'pending',
    STATUS_IN_PROGRESS => 'in_progress',
    STATUS_COMPLETED   => 'completed',
    STATUS_CANCELLED   => 'cancelled',
};

# Task priority constants
use constant {
    PRIORITY_LOW    => 'low',
    PRIORITY_MEDIUM => 'medium',
    PRIORITY_HIGH   => 'high',
    PRIORITY_URGENT => 'urgent',
};

sub new {
    my ($class, %args) = @_;
    
    my $now = gmtime()->datetime . 'Z';
    
    my $self = {
        id          => create_uuid_as_string(UUID_V4),
        title       => $args{title} || die "Title is required",
        description => $args{description} || '',
        status      => $args{status} || STATUS_PENDING,
        priority    => $args{priority} || PRIORITY_MEDIUM,
        tags        => $args{tags} || [],
        assigned_to => $args{assigned_to} || '',
        created_at  => $args{created_at} || $now,
        updated_at  => $args{updated_at} || $now,
    };
    
    bless $self, $class;
    return $self;
}

sub from_hash {
    my ($class, $hash) = @_;
    
    return $class->new(
        title       => $hash->{title},
        description => $hash->{description},
        status      => $hash->{status},
        priority    => $hash->{priority},
        tags        => $hash->{tags},
        assigned_to => $hash->{assigned_to},
    );
}

sub to_hash {
    my $self = shift;
    
    return {
        id          => $self->{id},
        title       => $self->{title},
        description => $self->{description},
        status      => $self->{status},
        priority    => $self->{priority},
        tags        => $self->{tags} || [],
        assigned_to => $self->{assigned_to},
        created_at  => $self->{created_at},
        updated_at  => $self->{updated_at},
    };
}

sub update {
    my ($self, $updates) = @_;
    
    $self->{title}       = $updates->{title}       if exists $updates->{title};
    $self->{description} = $updates->{description} if exists $updates->{description};
    $self->{status}      = $updates->{status}      if exists $updates->{status};
    $self->{priority}    = $updates->{priority}    if exists $updates->{priority};
    $self->{tags}        = $updates->{tags}        if exists $updates->{tags};
    $self->{assigned_to} = $updates->{assigned_to} if exists $updates->{assigned_to};
    
    $self->{updated_at} = gmtime()->datetime . 'Z';
    
    return $self;
}

sub string_to_status {
    my $str = shift;
    return STATUS_PENDING unless $str;
    
    my $lower = lc($str);
    return STATUS_IN_PROGRESS if $lower eq 'in_progress';
    return STATUS_COMPLETED   if $lower eq 'completed';
    return STATUS_CANCELLED   if $lower eq 'cancelled';
    return STATUS_PENDING;
}

sub string_to_priority {
    my $str = shift;
    return PRIORITY_MEDIUM unless $str;
    
    my $lower = lc($str);
    return PRIORITY_LOW    if $lower eq 'low';
    return PRIORITY_HIGH   if $lower eq 'high';
    return PRIORITY_URGENT if $lower eq 'urgent';
    return PRIORITY_MEDIUM;
}

1;