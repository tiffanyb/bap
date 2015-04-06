open Core_kernel.Std
open Cmdliner
open Bap.Std
open Bap_plugins.Std

let print_source = function
  | `bw -> print_endline "bw"
  | _ -> print_endline "else"

let source = [
  "BW", `bw;
  "SymTbl", `symtbl;
  "User", `user;
  "Ida", `ida;
  ]

let tool : _ Term.t =
  let doc = "The tool that we are going to evaluate" in
  Arg.(value & pos 1 (enum source) `bw & info [] ~docv:"tool" ~doc)

let gt : _ Term.t =
  let doc = "The ground truth source. One can direct to a user file, the symbol
  table" in
  Arg.(value & pos 2 (enum source) `symtbl & info [] ~docv:"gt" ~doc)

let symsfile : string option Term.t =
  let doc = "The symbol table of binaries. This requires the binaries Use this file as symbols source." in
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
  | `bw -> Func_start.byteweight bin
  | `user -> (match symsfile with
    | None -> (* TODO: return error using monad *) Addr.Hash_set.create ()
    | Some f -> Func_start.usersource f)
  | `symtbl -> Func_start.symbols bin
  | `ida -> Func_start.ida bin
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
  let metrics =
    if List.mem print_metrics `with_all then
      [`with_prec; `with_recl; `with_F; `with_TP; `with_FP; `with_FN]
    else print_metrics in
  let headers, items = List.fold metrics ~init:([], []) ~f:(fun (headers, items) -> function
    | `with_prec -> "Precision"::headers, (Printf.sprintf "%f" prec)::items
    | `with_recl -> "Recall"::headers, (Printf.sprintf "%f" recl)::items
    | `with_F -> "F_05"::headers, (Printf.sprintf "%f" f_05)::items
    | `with_TP -> "TP"::headers, (Printf.sprintf "%d" tp)::items
    | `with_FN -> "FN"::headers, (Printf.sprintf "%d" fn)::items
    | `with_FP -> "FP"::headers, (Printf.sprintf "%d" fp)::items
    (* TODO: return error.t *)
    | _ -> print_endline "error"; headers, items) in
  (* TODO: pretty printing *)
  Printf.printf "Tool\t%s\nBW\t%s\t\n" (String.concat ~sep:"\t" headers) (String.concat ~sep:"\t" items)

let compare_t = Term.(pure compare $bin $print_metrics $tool $gt $symsfile)

let info =
  let doc = "Bap-compare: to compare with the result against IDA Pro and the
  ground truth" in
  let man = [ `S "BUGS"; `P "Email bug reports"; ] in
  Term.info "bap-compare" ~version:"1.6.1" ~doc ~man

let () =
  Printexc.record_backtrace true;
  Plugins.load ();
  match Term.eval ~catch:false (compare_t, info) with
  | `Error _ -> exit 1
  | _ -> exit 0
