open Bap.Std

exception External_cmderr of string

(* [eval ~tool ~testbin] evaluates the tool on the testbin binary, and *)
(* returns the function start address set, and the tool's name for the *)
(* header of output table *)
val eval: tool:string -> testbin:string -> Addr.Set.t * string
