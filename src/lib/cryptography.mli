open! Core
open! Async
open! Import

val encrypt : key:string -> data:string -> string

val decrypt : key:string -> data:string -> string

val hash : string -> string
