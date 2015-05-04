open Core_kernel.Std
open Cmdliner
open Bap.Std
open Bap_plugins.Std

type gt_format = [ 
  | `unstripped_bin
  | `symbol_file ]

type result = {
  tool_name: string;
  tp: int;
  fn: int;
  fp: int;
  prec: float;
  recl: float;
  f_05: float}

let tool : string Term.t =
  let doc = "The tool of the function start result that we are going to
  evaluate. If user wants to evaluate bap-byteweight, one can use
  \"bap-byteweight\". If user want to evaluate IDA, one can use
  \"idaq\", \"idal\", \"idaq64\" or the specific path of IDA." in
  Arg.(required & pos 0 (some string) (Some "bap-byteweight") & info [] ~docv:"tool" ~doc)

let bin : string Term.t =
  let doc = "The testing stripped binary." in
  Arg.(required & pos 1 (some non_dir_file) None & info [] ~docv:"binary" ~doc)
 
let gt : string Term.t =
  let doc =
    "The ground truth. The ground truth can be an unstripped
  binary, or a .scm file with symbol information. In .scm file, each symbol should
  be in format of:
  (<symbol name> <symbol start address> <symbol end address>), e.g.
  (malloc@@GLIBC_2.4 0x11034 0x11038)" in
  Arg.(required & pos 2 (some non_dir_file) None
       & info [] ~docv:"ground-truth" ~doc)

let print_metrics : _ list Term.t =
  let opts = [
    "precision", `with_prec;
    "recall", `with_recl;
    "F_measure", `with_F;
    "TP", `with_TP;
    "FP", `with_FP;
    "FN", `with_FN;
  ] in
  let doc = "Print metrics. User can choose to print -cprecision, -crecall,
  -cF_measure, -cTP, -cFP and -cFN." in
  Arg.(value & opt_all (enum opts) (List.map ~f:snd opts) &
       info ["with-metrics"; "c"] ~doc)

let print {tool_name;fp;fn;tp;prec;recl;f_05} print_metrics =
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
    

let get_format f =
  if Filename.check_suffix f ".scm" then `symbol_file
  else `unstripped_bin


let compare tool bin gt print_metrics : unit = try
    let fs_tool, tool_name = Func_start.eval ~tool ~testbin:bin in
    let fs_gt = match get_format gt with
      | `unstripped_bin -> Ground_truth.from_unstripped_bin gt
      | `symbol_file -> Ground_truth.from_symbol_file gt ~testbin:bin in
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
    print {tool_name;fp;fn;tp;prec;recl;f_05} print_metrics
  with
  | Func_start.External_cmderr cmd ->
    Printf.printf
      "Function start is not recognized properly due to the following
  command cannot be executed properly: \n %s\n" cmd
  
let compare_t = Term.(pure compare $tool $bin $gt $print_metrics)

let info =
  let doc = "to compare the functions start identification result to the ground
  truth" in
  let man = [] in
  Term.info "bap-fsi-benchmark" ~doc ~man

let () =
  Printexc.record_backtrace true;
  Plugins.load ();
  match Term.eval ~catch:false (compare_t, info) with
  | `Error _ -> exit 1
  | _ -> exit 0
