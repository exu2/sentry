open! Core
open! Async
open Sentry_lib

let command =
  Command.async_or_error ~summary:""
    (let open Command.Let_syntax in
    let%map_open () = return () in
    fun () ->
      let open Deferred.Let_syntax in
      let%bind user =
        Interactive.ask_user
          "Enter your username in order to delete your account:"
      in
      let%bind master_password =
        Interactive.ask_user "Please type in your master password:"
      in
      match%bind
        Async_interactive.ask_yn
          "Are you sure you'd want to delete your account?"
      with
      | false -> Deferred.Or_error.ok_unit
      | true ->
          Sentry_server_connection.with_close ~f:(fun connection ->
              Rpc.Rpc.dispatch_exn Sentry_rpcs.remove_user_v1 connection
                { Sentry_rpcs.User_and_password.user; master_password }))
