open! Core
open! Async

type t =
  | Add_user of { user : string; hashed_master_password : string }
  | Remove_user of { user : string; hashed_master_password : string }
  | Add_entry of {
      user : string;
      hashed_master_password : string;
      entry : string;
      encrypted_password : string;
    }
  | Remove_entry of {
      user : string;
      hashed_master_password : string;
      entry : string;
    }
[@@deriving sexp]
