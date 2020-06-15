open! Core_kernel
open! Async_rpc_kernel

module User_and_password = struct
  module Stable = struct
    module V1 = struct
      type t = { user : string; password : string } [@@deriving bin_io, sexp]
    end
  end

  include Stable.V1
end

let add_user_v1 =
  Rpc.Rpc.create ~name:"add-user" ~version:1
    ~bin_query:User_and_password.Stable.V1.bin_t
    ~bin_response:(Or_error.Stable.V2.bin_t Unit.Stable.V1.bin_t)
