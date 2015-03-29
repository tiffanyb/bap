open Bap.Std
open Core_kernel.Std

let byteweight bin =
  let tmp = Filename.temp_file "bw_" ".output" in
  Printf.fprintf stderr "%s\n" tmp;
  let cmd = Printf.sprintf "bap-byteweight find %s > %s" bin tmp in
  Printf.fprintf stderr "%s\n" cmd;
  let _ = Unix.system cmd in
  Symbols.read_addrset ~filename:tmp

let usersource f =
  (* let s = Addr.Hash_set.of_list [Addr.of_string "0x1234:32"] in
  Symbols.write_addrset ~filename:f s; *)
  Symbols.read_addrset ~filename:f
