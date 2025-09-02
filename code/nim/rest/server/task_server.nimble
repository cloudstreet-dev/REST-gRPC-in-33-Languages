# Package

version       = "1.0.0"
author        = "David Liedle"
description   = "Task Management REST API Server in Nim"
license       = "MIT"
srcDir        = "src"
bin           = @["task_server"]

# Dependencies

requires "nim >= 2.0.0"
requires "jester >= 0.5.0"
requires "norm >= 2.6.0"
requires "karax >= 1.3.0"
requires "uuid4"