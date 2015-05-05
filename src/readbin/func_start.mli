open Bap.Std
open Core_kernel.Std
open Cmdliner

type eval_metrics

type print_metrics = [
  | `with_F
  | `with_FN
  | `with_FP
  | `with_TP
  | `with_prec
  | `with_recl
]

(* [print tool metrics print_metrics] prints out the evaluation *)
(* metrics of tool in terms of print_metrics *)
val print: string -> eval_metrics -> print_metrics list -> unit

(* [compare tool gt] compares the function start result of tool to *)
(* ground truth gt, and outputs the result to metrics, including true *)
(* positives, false positives, false negatives, precision, recall, *)
(* and the f_0.5 measurement *)
val compare: Addr.Set.t -> Addr.Set.t -> eval_metrics

(* [of_gt gt ~testbin] given the test binary testbin, returns the *)
(* function start address from ground truth gt *)
val of_gt: string -> testbin:string -> Addr.Set.t Or_error.t

(* [of_tool tool] returns the function start address from tool *)
val of_tool: string -> testbin:string -> Addr.Set.t Or_error.t
