open Core_kernel.Std
open Cmdliner
open Bap.Std
open Bap_plugins.Std

let tool : string Term.t =
  let doc = "The tool of the function start result that we are going to
  evaluate. If user wants to evaluate bap-byteweight, one can use
  \"bap-byteweight\". If user want to evaluate IDA, one can use
  \"idaq\", \"idal\", \"idaq64\" or the specific path of IDA." in
  Arg.(required & pos 0 (some string) (Some "bap-byteweight") & info [] ~docv:"tool" ~doc)

let bin : string Term.t =
  let doc = "The testing stripped binary." in
  Arg.(required & pos 1 (some non_dir_file) None & info [] ~docv:"binary" ~doc)

let truth : string Term.t =
  let doc =
    "The ground truth. The ground truth can be an unstripped
  binary, or a .scm file with symbol information. In .scm file, each symbol should
  be in format of:
  (<symbol name> <symbol start address> <symbol end address>), e.g.
  (malloc@@GLIBC_2.4 0x11034 0x11038)" in
  Arg.(required & pos 2 (some non_dir_file) None
       & info [] ~docv:"ground-truth" ~doc)

let print_metrics : Func_start.print_metrics list Term.t =
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

let compare tool bin truth print_metrics : unit =
  let open Or_error in
  match (Func_start.of_tool tool ~testbin:bin >>| fun fs_tool ->
         Func_start.of_gt truth ~testbin:bin >>| fun fs_gt ->
         let metrics = Func_start.compare fs_gt fs_tool in
         Func_start.print tool metrics print_metrics) with
  | Ok _ -> ()
  | Error err ->
    Format.printf "Function start is not recognized properly due to
  the following error:\n %a" Error.pp err


let compare_t = Term.(pure compare $tool $bin $truth $print_metrics)

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
