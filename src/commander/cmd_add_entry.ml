open! Core
open! Async
open Sentry_lib

let command =
  Command.async_or_error ~summary:""
    (let open Command.Let_syntax in
    let%map_open () = return () in
    fun () ->
      let open Deferred.Let_syntax in
      let%bind user = Interactive.ask_user "Enter your username:" in
      let%bind master_password =
        Interactive.ask_user "Please type in your master password:"
      in
      let%bind entry =
        Interactive.ask_user "Enter the name of the entry you'd like to use:"
      in
      (* TODO: randomly generate password *)
      let entry_password = Random.int 1000000000 |> string_of_int in
      printf !"Here's your generated password: %s\n" entry_password;
      match%bind Async_interactive.ask_yn "Confirm?" with
      | false -> Deferred.Or_error.ok_unit
      | true ->
          Sentry_server_connection.with_close ~f:(fun connection ->
              Rpc.Rpc.dispatch_exn Sentry_rpcs.add_password_entry_v1 connection
                {
                  Sentry_rpcs.Entry_info.user;
                  master_password;
                  entry;
                  entry_password;
                }))
