open Bap.Std
open Core_kernel.Std
open Or_error
(* File extension : scm : symbol table file *)
(* Binary: Unstripped binary *)

exception Not_implemented of string

type format = [
  | `unstripped_bin of string
  | `symbol_file of string ]


(* get_format : return the format of file *)
let get_format f =
  (* if the file is with extention .scm, then it is a symbol file *)
  if Filename.check_suffix f ".scm" then `symbol_file f
  else `unstripped_bin f
      

let with_file ~filename ~testbin : Addr.Set.t =
  match get_format filename with
  | `symbol_file f ->
    Image.create testbin >>| (fun (img, _errors) ->
    let arch = Image.arch img in
    Table.foldi (Image.sections img) ~init:Addr.Set.empty
      ~f:(fun mem sec t_fs ->
          if Section.is_executable sec then
            let sym_tbl = Symbols.read ~filename:f arch mem in
            Seq.fold ~init:t_fs (Table.regions sym_tbl)
              ~f:(fun accum mem -> Addr.Set.add accum @@ Memory.min_addr mem)
          else t_fs)) |> ok_exn
  | `unstripped_bin bin ->
    let tmp = Filename.temp_file "bw_" ".symbol" in
    let cmd = Printf.sprintf "bap-byteweight dump -i %s %S > %S" "SymTbl"
        bin tmp in
    (* TODO: how to deal with the process status? *)
    (match Unix.system cmd with
    | Unix.WEXITED _ -> Symbols.read_addrset tmp
    | Unix.WSIGNALED i
    | Unix.WSTOPPED i
      -> raise (Not_implemented
           "do not know how to deal with abnormal system exit"))
