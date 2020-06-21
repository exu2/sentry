open! Core
open! Async
include Transaction_log_intf

module Make (M : Transaction_log_state) = struct
  let filename = "transaction.log"

  module Service = struct
    type t = string

    let create () = M.rundir

    let init t =
      let state = M.create () in
      Writer.save (t ^/ filename) ~contents:(Sexp.to_string (M.sexp_of_t state))
  end

  let read_state service = Sexp.load_sexp (service ^/ filename) |> M.t_of_sexp

  let write_update service update =
    let state = read_state service in
    let state, result = M.apply_update state update in
    let%map () =
      Writer.save (service ^/ filename)
        ~contents:(Sexp.to_string (M.sexp_of_t state))
    in
    result
end
