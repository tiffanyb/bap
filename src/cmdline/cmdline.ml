open Core_kernel.Std
open Cmdliner

let revolt () = print_endline "Revolt!"

let revolt_t = Term.(pure revolt $ pure ())


let print_source = function
  | `bw -> print_endline "bw"
  | _ -> print_endline "else"


let compare print_metrics tool gt : unit =
  List.iter print_metrics ~f:(fun f -> match f with
    | `with_prec -> print_endline "with_prec"
    | `with_recl -> print_endline "with_recall"
    | `with_F -> print_endline "with_F"
    | `with_TP -> print_endline "with_TP"
    | `with_FN -> print_endline "with_FN"
    | _ -> print_endline "with_prec"
  );
  print_source tool;
  print_source gt

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
    "ALL", `with_all;
  ] in
  let doc = "doc" in
  Arg.(value & opt_all ~vopt:`with_all (enum opts) [] &
      info ["print-metrics"; "p"] ~doc)

let compare_t = Term.(pure compare $print_metrics $tool $gt)

let info =
  let doc = "aaa" in
  let man = [ `S "BUGS"; `P "Email bug reports"; ] in
  Term.info "chorus" ~version:"1.6.1" ~doc ~man

let () = match Term.eval (compare_t, info) with
  | `Error _ -> exit 1
  | _ -> exit 0
