open Bap.Std
open Core_kernel.Std

type gt_format = [
  | `unstripped_bin
  | `symbol_file ]

type eval_metrics = {
  true_positive: int;
  false_negative: int;
  false_positive: int;
  prec: float;
  recl: float;
  f_05: float}

type print_metrics = [
  | `with_F
  | `with_FN
  | `with_FP
  | `with_TP
  | `with_prec
  | `with_recl
]

let format_of_filename f =
  if Filename.check_suffix f ".scm" then `symbol_file
  else `unstripped_bin

let of_gt gt ~testbin : Addr.Set.t Or_error.t =  match format_of_filename gt with
  | `unstripped_bin -> Ground_truth.from_unstripped_bin gt
  | `symbol_file -> Ground_truth.from_symbol_file gt ~testbin


let of_tool tool ~testbin : Addr.Set.t Or_error.t =
  if tool = "bap-byteweight"
  then Find_starts.with_byteweight testbin
  else Find_starts.with_ida ~which_ida:tool testbin

let compare_against ~truth tool : eval_metrics =
  let false_positive = Set.(length (diff tool truth)) in
  let false_negative = Set.(length (diff truth tool)) in
  let true_positive = Set.length truth - false_negative in
  let ratio x = Float.(of_int true_positive / (of_int true_positive + of_int x)) in
  let prec = ratio false_positive in
  let recl = ratio false_negative in
  let f_05 = 1.5 *. prec *. recl /. (0.5 *. prec +. recl) in
  {false_positive;false_negative;true_positive;prec;recl;f_05}


let print tool {false_positive;false_negative;true_positive;prec;recl;f_05} print_metrics =
  let tool_name =
    if tool = "bap-byteweight" then "BW" else "IDA" in
  let headers, items =
    let rev_hd, rev_it = List.fold print_metrics ~init:(["Tool"], [tool_name])
        ~f:(fun (headers, items) -> function
            | `with_prec -> "Prcs"::headers, (Printf.sprintf "%.2g" prec)::items
            | `with_recl -> "Rcll"::headers, (Printf.sprintf "%.2g" recl)::items
            | `with_F -> "F_05"::headers, (Printf.sprintf "%.2g" f_05)::items
            | `with_TP -> "TP"::headers, (Printf.sprintf "%d" true_positive)::items
            | `with_FN -> "FN"::headers, (Printf.sprintf "%d" false_negative)::items
            | `with_FP -> "FP"::headers, (Printf.sprintf "%d" false_positive)::items ) in
    List.rev rev_hd, List.rev rev_it in
  Printf.printf "%s\n%s\n" (String.concat ~sep:"\t" headers)
    (String.concat ~sep:"\t" items)

