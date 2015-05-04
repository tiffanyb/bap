open Bap.Std

exception External_cmderr of string

(* eval : given the tool and test binary, output the function start *)
(* set and the tool's short name for output table header *)
(* [eval ~tool ~testbin] .... *)
val eval: tool:string -> testbin:string -> Addr.Set.t * string
