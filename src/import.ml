open! Core
open! Lwt

let ( !! ) = force

let ( let* ) = ( >>= )

let ( let+ ) = ( >|= )
