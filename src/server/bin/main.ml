open! Core
open! Async

let () =
  Command.run
    (Command.group ~summary:""
       [
         ("start", Sentry_server.start_command);
         ("init", Sentry_server.init_command);
       ])
