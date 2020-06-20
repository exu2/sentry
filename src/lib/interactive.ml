open! Core
open! Async

let ask_user str = Async_interactive.ask_dispatch_gen ~f:(fun x -> Ok x) str
