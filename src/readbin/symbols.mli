open Core_kernel.Std
open Bap.Std
(** reads symtab from a given file.
    File grammar:
    {[syms = sym, {sym};
      sym  = "(" name, addr, addr ")".]}

    Where the firs [addr] is the symbol start, end the
    last addr points to the next byte after the last.

    Example:
      (sub_AE8C 0xae8c 0xaecc)
      (sub_AED0 0xaed0 0xaf10)

    The file can be generated from IDA with the following script:

    Wait()
    fs = Functions(SegStart(ea), SegEnd(ea))
    for f in fs:
       print '(%s 0x%x 0x%x)' % (
         GetFunctionName(f),
         GetFunctionAttr(f, FUNCATTR_START),
         GetFunctionAttr(f, FUNCATTR_END))

    where [ea] is any address inside the section of interest.

*)

(* read symbol table from an input channel *)
val read : ?demangle:Options.demangle -> in_channel -> arch -> mem -> string table

(* read function start address set from an input channel *)
val read_addrset : in_channel -> Addr.Set.t

(* write function start address set from symbol table to an output channel *)
val write : out_channel -> symbol table -> unit

(* write function start addresses to output channel *)
val write_addrset : out_channel -> Addr.Set.t -> unit
