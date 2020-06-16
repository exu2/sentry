open! Core
open! Async

let command =
  Command.async_or_error ~summary:""
    (let open Command.Let_syntax in
    let%map_open () = return () in
    fun () ->
      let open Deferred.Let_syntax in
      let%bind user =
        Async_interactive.ask_dispatch_gen
          ~f:(fun x -> Ok x)
          "Choose a username:"
      in
      let%bind master_password =
        Async_interactive.ask_dispatch_gen
          ~f:(fun x -> Ok x)
          "Please choose and type in your master password:"
      in
      let%bind master_password' =
        Async_interactive.ask_dispatch_gen
          ~f:(fun x -> Ok x)
          "Please retype the password again:"
      in
      match String.equal master_password master_password' with
      | false -> Deferred.Or_error.errorf "Passwords did not match"
      | true -> (
          match%bind Async_interactive.ask_yn "Confirm?" with
          | false -> Deferred.Or_error.ok_unit
          | true ->
              Rpc.Rpc.dispatch_exn Sentry_rpcs.add_user_v1 connection
                { Sentry_rpcs.User_and_password.user; master_password } ))
