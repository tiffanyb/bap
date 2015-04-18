open Bap.Std
open Core_kernel.Std
open Or_error

exception Bad_user_input of string

let bap_byteweight bin subcmd =
  let tmp = Filename.temp_file "bw_" ".output" in
  let cmd = Printf.sprintf "bap-byteweight %s -x %S > %S" subcmd bin tmp in
  try
    let _ = Unix.system cmd in
    Symbols.read_addrset tmp
  with _ -> raise (Bad_user_input subcmd)

let byteweight bin = bap_byteweight bin "find"

let symbols bin = bap_byteweight bin "symbols"

let user = Symbols.read_addrset

let ida bin : Addr.Set.t =
  let roots_of_table t : addr list =
    Seq.(Table.regions t >>| Memory.min_addr |> to_list) in
  let res =
    Image.create ~backend:"llvm" bin >>= fun (img, _warns) ->
    let arch = Image.arch img in
    Ida.create ~ida:"idaq64" bin >>| fun ida ->
    Table.foldi (Image.sections img) ~init:[] ~f:(fun mem sec ida_syms ->
        if Section.is_executable sec then
          let ida_syms_t = roots_of_table Ida.(get_symbols ida arch mem) in
          ida_syms @ ida_syms_t
        else ida_syms) in
  match res with
  | Ok l -> Addr.Set.of_list l
  | Error err -> Printf.printf "IDA Error: %s\n" @@ Error.to_string_hum err; Addr.Set.of_list []
