open! Core
open! Async

let connect ~where_to_connect =
  let make_transport fd ~max_message_size =
    let info = Info.of_string "" in
    let reader = Reader.create fd |> Reader.pipe in
    let writer = Writer.create fd |> Writer.pipe in
    let private_key = Cryptography.Rsa.create () in
    let public_key = Cryptography.Rsa.public_key private_key in
    don't_wait_for
      (Pipe.write_if_open writer (Cryptography.Rsa.Public.to_string public_key));
    let%bind.Deferred.Or_error.Let_syntax other_side_public_key =
      match%bind Pipe.read reader with
      | `Ok a -> Cryptography.Rsa.Public.of_string a |> Deferred.Or_error.return
      | `Eof ->
          Deferred.Or_error.errorf
            "Did not receive public key from the server side"
    in
    let reader =
      Pipe.map reader ~f:(fun str -> Cryptography.Rsa.decrypt private_key str)
    in
    let writer =
      let r, w = Pipe.create () in
      don't_wait_for
        (Pipe.transfer r writer ~f:(fun str ->
             Cryptography.Rsa.encrypt other_side_public_key str));
      w
    in
    let%bind reader = Reader.of_pipe info reader in
    let%map writer, _ = Writer.of_pipe info writer in
    Rpc.Transport.of_reader_writer ~max_message_size reader writer
    |> Or_error.return
  in
  let handshake_timeout =
    Time_ns.Span.to_span_float_round_nearest
      Async_rpc_kernel.Async_rpc_kernel_private.default_handshake_timeout
  in
  let finish_handshake_by =
    Time_ns.add (Time_ns.now ())
      (Time_ns.Span.of_span_float_round_nearest handshake_timeout)
  in
  let%bind.Deferred.Or_error.Let_syntax sock =
    Monitor.try_with_or_error (fun () ->
        Tcp.connect_sock ~timeout:handshake_timeout where_to_connect)
  in
  let description =
    Info.create "Client connected via TCP" where_to_connect
      [%sexp_of: _ Tcp.Where_to_connect.t]
  in
  let handshake_timeout = Time_ns.diff finish_handshake_by (Time_ns.now ()) in
  let max_message_size = 100 * 1024 * 1024 in
  let%bind.Deferred.Or_error.Let_syntax transport =
    make_transport (Socket.fd sock) ~max_message_size
  in
  let {
    Async_rpc_kernel.Rpc.Connection.Client_implementations.connection_state;
    implementations;
  } =
    Async_rpc_kernel.Rpc.Connection.Client_implementations.null ()
  in
  match%bind
    Async_rpc_kernel.Rpc.Connection.create transport ~handshake_timeout
      ~description ~connection_state ~implementations
  with
  | Ok connection -> Deferred.return (Ok connection)
  | Error exn ->
      let%map () = Rpc.Transport.close transport in
      Or_error.of_exn exn

module Server = struct
  let create ~where_to_listen f =
    let%bind _ =
      Tcp.Server.create ~on_handler_error:`Ignore where_to_listen (fun _ r w ->
          let reader = Reader.pipe r in
          let writer = Writer.pipe w in
          let private_key = Cryptography.Rsa.create () in
          let public_key = Cryptography.Rsa.public_key private_key in
          don't_wait_for
            (Pipe.write_if_open writer
               (Cryptography.Rsa.Public.to_string public_key));
          let%bind other_side_public_key =
            ( match%bind Pipe.read reader with
            | `Ok a ->
                Cryptography.Rsa.Public.of_string a |> Deferred.Or_error.return
            | `Eof ->
                Deferred.Or_error.errorf
                  "Did not receive public key from the server side" )
            >>| ok_exn
          in
          let reader =
            Pipe.map reader ~f:(fun str ->
                Cryptography.Rsa.decrypt private_key str)
          in
          let writer =
            let r, w = Pipe.create () in
            don't_wait_for
              (Pipe.transfer r writer ~f:(fun str ->
                   Cryptography.Rsa.encrypt other_side_public_key str));
            w
          in
          let info = Info.of_string "" in
          let%bind r = Reader.of_pipe info reader in
          let%bind w, _ = Writer.of_pipe info writer in
          f r w)
    in
    Deferred.unit
end
