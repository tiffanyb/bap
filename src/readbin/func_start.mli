open Bap.Std

exception Bad_user_input of string

(* eval : given the tool and test binary, output the function start *)
(* set and the tool's short name for output table header *)
val eval: tool:string -> testbin:string -> Addr.Set.t * string
