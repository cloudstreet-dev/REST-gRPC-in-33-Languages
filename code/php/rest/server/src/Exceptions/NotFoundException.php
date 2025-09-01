<?php
declare(strict_types=1);

namespace TaskAPI\Exceptions;

use Exception;

class NotFoundException extends Exception
{
    public function __construct(string $message = "Resource not found", int $code = 404)
    {
        parent::__construct($message, $code);
    }
}