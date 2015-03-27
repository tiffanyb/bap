open Core_kernel.Std
open Cmdliner
open Bap.Std

let print_source = function
  | `bw -> print_endline "bw"
  | _ -> print_endline "else"

let source = [
  "BW", `bw;
  "SymTbl", `symbtl;]

let tool : _ Term.t =
  let doc = "The message to print." in
  Arg.(value & pos 0 (enum source) `symbtl & info [] ~docv:"tool" ~doc)

let gt : _ Term.t =
  let doc = "The message to print." in
  Arg.(value & pos 1 (enum source) `symbtl & info [] ~docv:"gt" ~doc)

let print_metrics : _ list Term.t =
  let opts = [
    "precision", `with_prec;
    "recall", `with_recl;
    "F_measure", `with_F;
    "TP", `with_TP;
    "FP", `with_FP;
    "FN", `with_FN;
    "all", `with_all;
  ] in
  let doc = "doc" in
  Arg.(value & opt_all ~vopt:`with_all (enum opts) [] &
      info ["print-metrics"; "p"] ~doc)

let func_start source : Addr.Hash_set.t = Addr.Hash_set.create ()

let compare print_metrics tool gt : unit =
  let fs_tool = func_start tool in
  let fs_gt = func_start gt in
  let fp =
    let set = Hash_set.diff fs_tool fs_gt in
    Hash_set.length set in
  let fn =
    let set = Hash_set.diff fs_gt fs_tool in
    Hash_set.length set in
  let tp = Hash_set.length fs_gt - fn in
  let prec, recl =
    let f_fp = float fp in
    let f_fn = float fn in
    let f_tp = float tp in
    f_tp /. (f_tp +. f_fp), f_tp /. (f_tp +. f_fn) in
  let f_05 = 1.5 *. prec *. recl /. (0.5 *. prec +. recl) in

  (* print out the metrics *)
  if List.mem print_metrics `with_all then
    print_endline "all"
  else List.iter print_metrics ~f:(fun f -> match f with
    | `with_prec -> print_endline "with_prec"
    | `with_recl -> print_endline "with_recall"
    | `with_F -> print_endline "with_F"
    | `with_TP -> print_endline "with_TP"
    | `with_FN -> print_endline "with_FN"
    | _ -> print_endline "with_prec")

let compare_t = Term.(pure compare $print_metrics $tool $gt)

let info =
  let doc = "aaa" in
  let man = [ `S "BUGS"; `P "Email bug reports"; ] in
  Term.info "bap-compare" ~version:"1.6.1" ~doc ~man

let () = match Term.eval (compare_t, info) with
  | `Error _ -> exit 1
  | _ -> exit 0
