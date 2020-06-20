open! Core
open! Async
open! Import

let trim_or_pad string =
  match String.length string with
  | 32 -> string
  | length when length < 32 ->
      let pad_string =
        List.map (List.range 0 (32 - length)) ~f:(fun _ -> " ")
        |> String.concat ~sep:""
      in
      String.concat ~sep:"" [ string; pad_string ]
  | _ -> String.sub ~pos:0 ~len:32 string

let cipher ~direction ~key ~data =
  (* TODO: just a hack to make it work. Need to be fixed *)
  let key = trim_or_pad key in
  let data = trim_or_pad data in
  let transform = Cipher.aes key direction in
  transform#put_string data;
  transform#finish;
  transform#get_string

let encrypt ~key ~data = cipher ~direction:Cipher.Encrypt ~key ~data

let decrypt ~key ~data = cipher ~direction:Cipher.Decrypt ~key ~data

let hash string = Sha256.string string |> Sha256.to_bin
