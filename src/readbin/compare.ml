open Core_kernel.Std
open Cmdliner
open Bap.Std
open Bap_plugins.Std

exception No_unstripped_file

type result = {
  tool_name: string;
  tp: int;
  fn: int;
  fp: int;
  prec: float;
  recl: float;
  f_05: float}

let tool : string Term.t =
  let doc = "The source of the function start result that we are going to
  evaluate. The default value is bap-byteweight. User can also use
  \"idaq\", \"idal\", \"idaq64\" or the specific path of IDA. " in
  Arg.(required & pos 0 (some string) (Some "bap-byteweight") & info [] ~docv:"tool" ~doc)

let bin : string Term.t =
  let doc = "The testing stripped binary." in
  Arg.(required & pos 1 (some non_dir_file) None & info [] ~docv:"binary" ~doc)
 
let gt : string Term.t =
  let doc =
    "The ground truth file. If the ground truth is from symbol table, this
  file should be an unstripped binary. If the ground truth is from
  user's input, this file should be a file with content in
  S-expression format." in
  Arg.(required & pos 2 (some non_dir_file) None
       & info [] ~docv:"ground truth file" ~doc)

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
      (* print out the metrics *)
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


let compare tool bin gt print_metrics : unit = try
    let fs_tool, tool_name = Func_start.eval ~tool ~testbin:bin in
    let fs_gt = Ground_truth.with_file ~filename:gt ~testbin:bin in
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
  | Func_start.Bad_user_input tool ->
    Printf.printf "bap-byteweight dump %s command does not \
                   work properly.\n" tool
  | Not_found -> Printf.printf "No Symbol File found.\n"
  | No_unstripped_file ->
    Printf.printf "Cannot get symbole table: No unstripped file found. Did you
    provide unstripped binary by -u?\n"

let compare_t = Term.(pure compare $tool $bin $gt $print_metrics)

let info =
  let doc = "to compare the functions start identification result to the ground
  truth" in
  let man = [] in
  Term.info "bap-measure-byteweight" ~doc ~man

let () =
  Printexc.record_backtrace true;
  Plugins.load ();
  match Term.eval ~catch:false (compare_t, info) with
  | `Error _ -> exit 1
  | _ -> exit 0
