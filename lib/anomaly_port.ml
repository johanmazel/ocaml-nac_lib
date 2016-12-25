
open Printf

module A = BatArray
module L = BatList

open Sexplib.Std
open Bin_prot.Std

open Key_occurrence_distribution_instantiations

type t =
  {
    src_port_distribution_compact : Int_distribution.C_compact.t;
    dst_port_distribution_compact : Int_distribution.C_compact.t;
  }
[@@deriving compare, sexp, bin_io]

let new_t
    src_port_distribution_compact
    dst_port_distribution_compact
    =
  {
    src_port_distribution_compact;
    dst_port_distribution_compact;
  }

let lengths t = (Int_distribution.C_compact.length t.src_port_distribution_compact, Int_distribution.C_compact.length t.dst_port_distribution_compact)

let of_detailed_metrics
    detailed_metrics
  =
  let src_port, dst_port =
    Detailed_metrics.get_src_dst_port_distribution
      detailed_metrics
  in

  let src_port_distribution_compact = Int_distribution.C_compact.of_c src_port in
  let dst_port_distribution_compact = Int_distribution.C_compact.of_c dst_port in

  let src_port_distribution = Int_distribution.C_compact.to_c src_port_distribution_compact in
  let dst_port_distribution = Int_distribution.C_compact.to_c dst_port_distribution_compact in

  assert(
    Int_distribution.C.compare
      src_port
      src_port_distribution
    =
    0
  );
  assert(
    Int_distribution.C.compare
      dst_port
      dst_port_distribution
    =
    0
  );

  new_t
    src_port_distribution_compact
    dst_port_distribution_compact
