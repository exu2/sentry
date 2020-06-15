open! Core
open! Async

module User_and_password : sig
  type t = { user : string; password : string } [@@deriving bin_io, sexp]

  module Stable : sig
    module V1 : sig
      type nonrec t = t
    end
  end
end

val add_user_v1 : (User_and_password.t, unit Or_error.t) Rpc.Rpc.t
