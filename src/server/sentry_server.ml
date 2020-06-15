open! Core
open! Async
open Sentry_lib
open Sentry_state

type t = { tlog_service : Tlog.Service.t }

let create ~rundir = { tlog_service = Tlog.Service.create ~rundir }

let add_user_v1 t { Sentry_rpcs.User_and_password.user; master_password } =
  let hashed_master_password = Cryptography.hash master_password in
  Tlog.write_update t.tlog_service
    (Update.Add_user { user; hashed_master_password })

let remove_user_v1 t { Sentry_rpcs.User_and_password.user; master_password } =
  let hashed_master_password = Cryptography.hash master_password in
  Tlog.write_update t.tlog_service
    (Update.Remove_user { user; hashed_master_password })

let add_password_entry_v1 t
    { Sentry_rpcs.Entry_info.user; master_password; entry; entry_password } =
  let hashed_master_password = Cryptography.hash master_password in
  let encrypted_password =
    Cryptography.encrypt ~key:master_password ~data:entry_password
  in
  Tlog.write_update t.tlog_service
    (Update.Add_entry
       { user; hashed_master_password; entry; encrypted_password })

let remove_password_entry_v1 t
    { Sentry_rpcs.Entry_info.user; master_password; entry; entry_password = _ }
    =
  let hashed_master_password = Cryptography.hash master_password in
  Tlog.write_update t.tlog_service
    (Update.Remove_entry { user; hashed_master_password; entry })

let get_password_entry_v1 t
    { Sentry_rpcs.Entry_info.user; master_password; entry; entry_password = _ }
    =
  let hashed_master_password = Cryptography.hash master_password in
  let state = Tlog.read_state t.tlog_service in
  State.lookup_password state ~user ~hashed_master_password ~entry
  |> Deferred.return

let implementations =
  let implementations =
    [
      Rpc.Rpc.implement Sentry_rpcs.add_user_v1 add_user_v1;
      Rpc.Rpc.implement Sentry_rpcs.remove_user_v1 remove_user_v1;
      Rpc.Rpc.implement Sentry_rpcs.add_password_entry_v1 add_password_entry_v1;
      Rpc.Rpc.implement Sentry_rpcs.remove_password_entry_v1
        remove_password_entry_v1;
      Rpc.Rpc.implement Sentry_rpcs.get_password_entry_v1 get_password_entry_v1;
    ]
  in
  Rpc.Implementations.create_exn ~implementations
    ~on_unknown_rpc:`Close_connection

let command =
  Command.async ~summary:"Sentry server"
    (let open Command.Let_syntax in
    let%map_open () = return ()
    and port =
      flag "port" (required int)
        ~doc:"PORT port number that the server will accept request in"
    and rundir =
      flag "rundir" (required string) ~doc:"(* TODO: make a default for this *)"
    in
    fun () ->
      let open Deferred.Let_syntax in
      let%bind _ =
        Tcp.Server.create ~on_handler_error:`Ignore
          (Tcp.Where_to_listen.of_port port) (fun _ r w ->
            Rpc.Connection.server_with_close r w ~implementations
              ~connection_state:(fun (_ : Rpc.Connection.t) -> create ~rundir)
              ~on_handshake_error:`Ignore)
      in
      Deferred.never ())
