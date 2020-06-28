open! Core
open! Async
open! Import

module Aes = struct
  let cipher ~direction ~key ~data =
    let block_size = 16 in
    let next_fit length =
      match length % block_size with
      | 0 -> length
      | _ -> length - (length % block_size) + block_size
    in
    let trim string =
      let next_fit_length = next_fit (String.length string) in
      match Int.equal (String.length string) next_fit_length with
      | true -> string
      | false ->
          let pad_string =
            List.map
              (List.range 0 (next_fit_length - String.length string))
              ~f:(fun _ -> " ")
            |> String.concat ~sep:""
          in
          String.concat ~sep:"" [ string; pad_string ]
    in
    let key = trim key in
    let data = trim data in
    let transform = Cipher.aes key direction in
    transform#put_string data;
    transform#finish;
    transform#get_string

  let encrypt ~key ~data = cipher ~direction:Cipher.Encrypt ~key ~data

  let decrypt ~key ~data = cipher ~direction:Cipher.Decrypt ~key ~data

  let hash string = Sha256.string string |> Sha256.to_bin
end

module Rsa = struct
  type 'a t = {
    size : int;  (** Size of the modulus [n], in bits *)
    n : string;  (** Modulus [n = p.q] *)
    e : string;  (** Public exponent [e] *)
    d : string;  (** Private exponent [d] *)
    p : string;  (** Prime factor [p] of [n] *)
    q : string;  (** The other prime factor [q] of [n] *)
    dp : string;  (** [dp] is [d mod (p-1)] *)
    dq : string;  (** [dq] is [d mod (q-1)] *)
    qinv : string;  (** [qinv] is a multiplicative inverse of [q] modulo [p] *)
  }
  [@@deriving sexp, bin_io]

  module Public = struct
    type nonrec t = [ `Public ] t [@@deriving sexp, bin_io]

    let to_string t = Sexp.to_string (sexp_of_t t)

    let of_string str = t_of_sexp (Sexp.of_string str)
  end

  module Private = struct
    type nonrec t = [ `Private ] t
  end

  let from_rsa_key { RSA.size; n; e; d; p; q; dp; dq; qinv } =
    { size; n; e; d; p; q; dp; dq; qinv }

  let to_rsa_key { size; n; e; d; p; q; dp; dq; qinv } =
    { RSA.size; n; e; d; p; q; dp; dq; qinv }

  let create () = RSA.new_key 2048 |> from_rsa_key

  let public_key t =
    {
      size = t.size;
      n = t.n;
      e = t.e;
      d = "";
      p = "";
      q = "";
      dp = "";
      dq = "";
      qinv = "";
    }

  let encrypt t = RSA.encrypt (to_rsa_key t)

  let decrypt t = RSA.decrypt (to_rsa_key t)
end
