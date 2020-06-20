open! Core
open! Async
open Sentry_lib

let command =
  Command.async_or_error ~summary:""
    (let open Command.Let_syntax in
    let%map_open () = return () in
    fun () ->
      let open Deferred.Let_syntax in
      let%bind user = Interactive.ask_user "Choose a username:" in
      let%bind master_password =
        Interactive.ask_user "Please choose and type in your master password:"
      in
      let%bind entry =
        Interactive.ask_user "Enter the name of the entry you'd like to delete:"
      in
      match%bind Async_interactive.ask_yn "Confirm?" with
      | false -> Deferred.Or_error.ok_unit
      | true ->
          Sentry_server_connection.with_close ~f:(fun connection ->
              Rpc.Rpc.dispatch_exn Sentry_rpcs.remove_password_entry_v1
                connection
                {
                  Sentry_rpcs.Entry_info.user;
                  master_password;
                  entry;
                  entry_password = "";
                }))
