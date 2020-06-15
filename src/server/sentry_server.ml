open! Core
open! Async
open Sentry_lib
open Sentry_state

type t = { tlog_service : Tlog.Service.t }

let create ~rundir = { tlog_service = Tlog.Service.create ~rundir }

let add_user_v1 t { Sentry_rpcs.User_and_password.user; password } =
  let hashed_master_password = Cryptography.hash password in
  Tlog.write_update t.tlog_service
    (Update.Add_user { user; hashed_master_password })

let implementations =
  let implementations =
    [ Rpc.Rpc.implement Sentry_rpcs.add_user_v1 add_user_v1 ]
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
