open! Core
open! Async
open Sentry_lib

let where_to_connect =
  Tcp.Where_to_connect.of_host_and_port
    (Host_and_port.of_string "localhost:8080")

let close connection = Rpc.Connection.close connection

let with_close ~f =
  let open Deferred.Or_error.Let_syntax in
  let%bind connection = Secure_connection.connect ~where_to_connect in
  Monitor.protect (fun () -> f connection) ~finally:(fun () -> close connection)

let with_close' ~f =
  with_close ~f:(fun connection -> f connection |> Deferred.ok)
