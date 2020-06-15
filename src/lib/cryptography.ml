open! Core
open! Async
open! Import

let cipher ~direction ~key ~data =
  let transform = Cipher.aes key direction in
  transform#put_string data;
  transform#get_string

let encrypt ~key ~data = cipher ~direction:Cipher.Encrypt ~key ~data

let decrypt ~key ~data = cipher ~direction:Cipher.Decrypt ~key ~data

let hash string = Sha256.string string |> Sha256.to_bin
