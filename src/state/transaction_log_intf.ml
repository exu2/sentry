open! Core
open! Async

(* Poor man's transaction log. Simple data replication in a specified location *)
module type Transaction_log_state = sig
  type t [@@deriving sexp]

  val create : unit -> t

  module Update : sig
    type t
  end

  module Update_result : sig
    type t
  end

  val apply_update : t -> Update.t -> t * Update_result.t
end

module type Transaction_log = sig
  module type Transaction_log_state = Transaction_log_state

  module Make (M : Transaction_log_state) : sig
    module Service : sig
      type t

      val create : rundir:string -> t

      val init : t -> unit Deferred.t
    end

    val read_state : Service.t -> M.t

    val write_update : Service.t -> M.Update.t -> M.Update_result.t Deferred.t
  end
end
