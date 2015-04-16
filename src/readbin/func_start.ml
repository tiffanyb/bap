open Bap.Std
open Core_kernel.Std
open Or_error

exception Bad_user_input

let byteweight bin =
  let tmp = Filename.temp_file "bw_" ".output" in
  let cmd = Printf.sprintf "bap-byteweight find -x %s > %s" bin tmp in
  let _ = Unix.system cmd in
  Symbols.read_addrset tmp

let usersource = Symbols.read_addrset

let symbols bin =
  let tmp = Filename.temp_file "bw_" ".output" in
  let cmd = Printf.sprintf "bap-byteweight symbols -x %s > %s" bin tmp in
  let _ = Unix.system cmd in
  try
    Symbols.read_addrset tmp
  with _ -> raise Bad_user_input

let ida bin : Addr.Hash_set.t =
  (* Strip the binary if it is not stripped yet *)
  let roots_of_table t : addr list =
    Seq.(Table.regions t >>| Memory.min_addr |> to_list) in
  let res =
    let stripped_bin =
      let basename = Filename.basename bin in
      Filename.temp_file basename ".stripped" in
    let cmd = Printf.sprintf "x86_64-elf-strip %s -o %s" bin stripped_bin in
    let _ = Unix.system cmd in
    Image.create ~backend:"llvm" stripped_bin >>= fun (img, _warns) ->
    let arch = Image.arch img in
    Ida.create ~ida:"idaq64" stripped_bin >>| fun ida ->
    Table.foldi (Image.sections img) ~init:[] ~f:(fun mem sec ida_syms ->
        if Section.is_executable sec then
          let ida_syms_t = roots_of_table Ida.(get_symbols ida arch mem) in
          ida_syms @ ida_syms_t
        else ida_syms) in
  match res with
  | Ok l -> Addr.Hash_set.of_list l
  | Error err -> Printf.printf "IDA Error: %s\n" @@ Error.to_string_hum err; Addr.Hash_set.of_list []
