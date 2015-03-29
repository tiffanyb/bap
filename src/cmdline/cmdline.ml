open Core_kernel.Std
open Cmdliner
open Bap.Std

let print_source = function
  | `bw -> print_endline "bw"
  | _ -> print_endline "else"

let source = [
  "BW", `bw;
  "SymTbl", `symbtl;
  "User", `user;
  ]

let tool : _ Term.t =
  let doc = "The message to print." in
  Arg.(value & pos 1 (enum source) `symbtl & info [] ~docv:"tool" ~doc)

let gt : _ Term.t =
  let doc = "The message to print." in
  Arg.(value & pos 2 (enum source) `symbtl & info [] ~docv:"gt" ~doc)

let symsfile : string option Term.t =
  let doc = "Use this file as symbols source." in
  Arg.(value & opt (some non_dir_file) None & info ["syms"; "s"]
  ~docv:"syms" ~doc)

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

let bin : string Term.t =
  let doc = "binary" in
  Arg.(required & pos 0 (some non_dir_file) None & info [] ~docv:"binary" ~doc)

let func_start bin symsfile : _ -> Addr.Hash_set.t = function
  | `bw -> Printf.printf "asdfasdf"; Func_start.byteweight bin
  | `user -> (match symsfile with
    | None -> (* TODO: return error using monad *) Printf.printf "aaa"; Addr.Hash_set.create ()
    | Some f -> Func_start.usersource f)
  | _ -> Addr.Hash_set.create ()

let compare bin print_metrics tool gt symsfile : unit =
  let fs_tool = func_start bin symsfile tool in
  let fs_gt = func_start bin symsfile gt in
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
  (* TODO: pretty printing *)
  if List.mem print_metrics `with_all then
    Printf.printf "%f %f %f\n" prec recl f_05
  else List.iter print_metrics ~f:(fun f -> match f with
    | `with_prec -> print_endline "with_prec"
    | `with_recl -> print_endline "with_recall"
    | `with_F -> print_endline "with_F"
    | `with_TP -> print_endline "with_TP"
    | `with_FN -> print_endline "with_FN"
    | _ -> print_endline "with_prec")

let compare_t = Term.(pure compare $bin $print_metrics $tool $gt $symsfile)

let info =
  let doc = "aaa" in
  let man = [ `S "BUGS"; `P "Email bug reports"; ] in
  Term.info "bap-compare" ~version:"1.6.1" ~doc ~man

let () = match Term.eval (compare_t, info) with
  | `Error _ -> exit 1
  | _ -> exit 0
