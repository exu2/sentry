open! Core
open! Async
include Random

let random_string ~length =
  let rand_int () = Random.int_incl 33 122 in
  List.map (List.range 0 length) ~f:(fun (_ : int) ->
      rand_int () |> Char.of_int_exn)
  |> String.of_char_list
