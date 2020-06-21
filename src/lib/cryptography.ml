open! Core
open! Async
open! Import

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
