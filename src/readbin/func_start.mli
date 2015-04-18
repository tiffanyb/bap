open Bap.Std

exception Bad_user_input of string

val byteweight : string -> Addr.Set.t
val user : string -> Addr.Set.t
val symbols : string -> Addr.Set.t
val ida : string -> Addr.Set.t
