open Task_server

let () =
  Printf.printf "🚀 OCaml Task REST API Server\n";
  Printf.printf "📍 Listening on http://localhost:8080\n";
  Printf.printf "🔍 Health check: http://localhost:8080/health\n\n%!";
  
  Dream.run ~port:8080
  @@ Dream.logger
  @@ Dream.router Routes.all
  @@ Dream.not_found