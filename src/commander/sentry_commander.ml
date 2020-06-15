open! Core
open! Async

let command =
  Command.group ~summary:"Sentry is a password management server"
    [
      ("add", Cmd_add.command);
      ("remove", Cmd_remove.command);
      ("add-entry", Cmd_add_entry.command);
      ("remove-entry", Cmd_remove_entry.command);
      ("get-entry", Cmd_get_entry.command);
    ]
