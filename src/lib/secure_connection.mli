open! Core
open! Async

val connect :
  where_to_connect:Tcp.Where_to_connect.inet ->
  Rpc.Connection.t Deferred.Or_error.t

module Server : sig
  val create :
    ?private_key:Cryptography.Rsa.Private.t ->
    where_to_listen:Tcp.Where_to_listen.inet ->
    (Reader.t -> Writer.t -> unit Deferred.t) ->
    unit Deferred.t
end
