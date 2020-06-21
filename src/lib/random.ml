open! Core
open! Async
include Random

let substitute_one_random_character string ~rand_int ~excluding_indices =
  let rec index_loop () =
    let index = Random.int (String.length string) in
    if Hash_set.mem excluding_indices index then index_loop () else index
  in
  let index = index_loop () in
  Hash_set.add excluding_indices index;
  String.mapi string ~f:(fun i char ->
      if Int.equal i index then rand_int () |> Char.of_int_exn else char)

let random_string' ~length ~rand_int =
  List.map (List.range 0 length) ~f:(fun (_ : int) ->
      rand_int () |> Char.of_int_exn)
  |> String.of_char_list

let random_string ?must_include_numbers ?must_include_uppercase_letters
    ?must_include_lowercase_letters ~length () =
  let open Or_error.Let_syntax in
  let string =
    random_string' ~length ~rand_int:(fun () -> Random.int_incl 33 122)
  in
  let rand_int_generators =
    [
      (* TODO: don't hardcode 33 and 122 *)
      (must_include_numbers, fun () -> Random.int_incl 48 57);
      (must_include_lowercase_letters, fun () -> Random.int_incl 97 122);
      (must_include_uppercase_letters, fun () -> Random.int_incl 65 90);
    ]
    |> List.filter ~f:(fun (requirement, _) -> Option.is_some requirement)
    |> List.map ~f:snd
  in
  let%map () =
    let number_of_requirements = List.length rand_int_generators in
    if number_of_requirements > length then
      Or_error.errorf
        !"Length (%d) must be greater than the total number of requirements \
          (%d)"
        length number_of_requirements
    else Ok ()
  in
  let excluding_indices = Int.Hash_set.create () in
  List.fold rand_int_generators ~init:string ~f:(fun string rand_int ->
      substitute_one_random_character string ~rand_int ~excluding_indices)
