open! Core
open! Async

let where_to_connect =
  Tcp.Where_to_connect.of_host_and_port
    (Host_and_port.of_string "localhost:8080")

let connect () =
  Rpc.Connection.client where_to_connect |> Deferred.Or_error.of_exn_result

let close connection = Rpc.Connection.close connection

let with_close ~f =
  let open Deferred.Or_error.Let_syntax in
  let%bind connection = connect () in
  Monitor.protect (fun () -> f connection) ~finally:(fun () -> close connection)

let with_close' ~f =
  with_close ~f:(fun connection -> f connection |> Deferred.ok)
