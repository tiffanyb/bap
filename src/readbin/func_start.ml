open Bap.Std
open Core_kernel.Std

type gt_format = [ 
  | `unstripped_bin
  | `symbol_file ]

type metrics = {
  tp: int;
  fn: int;
  fp: int;
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
  else Find_starts.with_ida ~whichida:tool testbin


let compare fs_tool fs_gt : metrics =
  let fp =
    let set = Set.diff fs_tool fs_gt in
    Set.length set in
  let fn =
    let set = Set.diff fs_gt fs_tool in
    Set.length set in
  let tp = Set.length fs_gt - fn in
  let prec, recl =
    let f_fp = float fp in
    let f_fn = float fn in
    let f_tp = float tp in
    f_tp /. (f_tp +. f_fp), f_tp /. (f_tp +. f_fn) in
  let f_05 = 1.5 *. prec *. recl /. (0.5 *. prec +. recl) in
  {fp;fn;tp;prec;recl;f_05}


let print tool {fp;fn;tp;prec;recl;f_05} print_metrics =
  let tool_name =
    if tool = "bap-byteweight" then "BW" else "IDA" in
  let headers, items =
    let rev_hd, rev_it = List.fold print_metrics ~init:(["Tool"], [tool_name])
        ~f:(fun (headers, items) -> function
            | `with_prec -> "Prcs"::headers, (Printf.sprintf "%.2g" prec)::items
            | `with_recl -> "Rcll"::headers, (Printf.sprintf "%.2g" recl)::items
            | `with_F -> "F_05"::headers, (Printf.sprintf "%.2g" f_05)::items
            | `with_TP -> "TP"::headers, (Printf.sprintf "%d" tp)::items
            | `with_FN -> "FN"::headers, (Printf.sprintf "%d" fn)::items
            | `with_FP -> "FP"::headers, (Printf.sprintf "%d" fp)::items ) in
    List.rev rev_hd, List.rev rev_it in
  Printf.printf "%s\n%s\n" (String.concat ~sep:"\t" headers)
    (String.concat ~sep:"\t" items)
    
