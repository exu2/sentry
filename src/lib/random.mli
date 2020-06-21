open! Core
open! Async

include module type of Random

val random_string : length:int -> string
