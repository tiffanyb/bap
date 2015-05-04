open Bap.Std
open Core_kernel.Std
open Or_error

exception External_cmderr of string

let byteweight bin =
  let tmp = Filename.temp_file "bw_" ".output" in
  let cmd = Printf.sprintf "bap-byteweight dump -i byteweight %S > %S" bin tmp in
  let return = Sys.command cmd in
  if return = 0 then
    In_channel.with_file tmp ~f:Symbols.read_addrset
  else raise (External_cmderr cmd)


let ida ?which_ida bin : Addr.Set.t =
  let res =
    Image.create bin >>= fun (img, _warns) ->
    let arch = Image.arch img in
    Ida.create ?ida:which_ida bin >>| fun ida ->
    Table.foldi (Image.sections img) ~init:Addr.Set.empty ~f:(fun mem sec ida_syms ->
        if Section.is_executable sec then
          let sym_tbl = Ida.(get_symbols ida arch mem) in
          Seq.fold Seq.(Table.regions sym_tbl >>| Memory.min_addr)
            ~init:ida_syms
            ~f:(fun s sym -> Addr.Set.add s sym)
        else ida_syms) in
  match res with
  | Ok l -> l
  | Error err -> raise (External_cmderr (Error.to_string_hum err))

let eval ~tool ~testbin : Addr.Set.t * string = match tool with
  | "bap-byteweight" -> byteweight testbin, "BW"
  | i -> ida ?which_ida:(Some i) testbin, "IDA"
