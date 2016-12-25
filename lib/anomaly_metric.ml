
open Printf

open Sexplib.Std
open Bin_prot.Std

type t =
  {
    flow : float;
    packet : float;
    byte : float;
  }
[@@deriving compare, sexp, bin_io]

let new_t
    flow
    packet
    byte
    =
  {
    flow;
    packet;
    byte;
  }

let new_empty_t () = new_t 0. 0. 0.

(* let to_string to_string_mode t = *)
(*   "" *)
