open Bap.Std
open Core_kernel.Std
open Or_error      

exception External_cmderr of string

let from_unstripped_bin bin =
  let tmp = Filename.temp_file "bw_" ".symbol" in
  let cmd = Printf.sprintf "bap-byteweight dump -i %s %S > %S" "symbols"
      bin tmp in
  let return = Sys.command cmd in
  if return = 0
  then In_channel.with_file tmp ~f:Symbols.read_addrset
  else raise (External_cmderr cmd)


let from_symbol_file filename ~testbin : Addr.Set.t =
  Image.create testbin >>| (fun (img, _errors) ->
      let arch = Image.arch img in
      Table.foldi (Image.sections img) ~init:Addr.Set.empty
        ~f:(fun mem sec t_fs ->
            if Section.is_executable sec then
              let sym_tbl = In_channel.with_file filename
                  ~f:(fun ic -> Symbols.read ic arch mem) in
              Seq.fold ~init:t_fs (Table.regions sym_tbl)
                ~f:(fun accum mem -> Addr.Set.add accum @@ Memory.min_addr mem)
            else t_fs)) |> ok_exn
  
