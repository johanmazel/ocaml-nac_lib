
open Printf

module A = BatArray
module L = BatList

open Sexplib.Std
open Bin_prot.Std

(* open Key_occurrence_distribution_instantiations *)
open Network_traffic_metric_instantiations
  
(* open Ip_address_data_structures_instantiations *)
    
type t =
  {
    src_ip_address_container : Ip_address_container_compact.t;
    dst_ip_address_container : Ip_address_container_compact.t;
  }
with compare, sexp, bin_io

let new_t
    src_ip_address_container
    dst_ip_address_container
    =
  {
    src_ip_address_container;
    dst_ip_address_container;
  }

let lengths t = (Ip_address_container_compact.length t.src_ip_address_container, Ip_address_container_compact.length t.dst_ip_address_container)

let of_detailed_metrics
    detailed_metrics
  =
  let src_ip_address_container =
    Ip_address_container_compact.of_ipaddr_list
      (L.map
          Admd.Ipaddr_sb.to_ipaddr
         (L.map
            fst
      (Ipaddr_distribution.C.bindings detailed_metrics.Detailed_metrics.src_addr)
         )
      )
  in
  let dst_ip_address_container =
    Ip_address_container_compact.of_ipaddr_list
      (L.map
          Admd.Ipaddr_sb.to_ipaddr
         (L.map
            fst
      (Ipaddr_distribution.C.bindings detailed_metrics.Detailed_metrics.dst_addr)
         )
      )
  in
  new_t
    src_ip_address_container
    dst_ip_address_container

let inter t1 t2 =
  new_t
    (Ip_address_container_compact.inter t1.src_ip_address_container t2.src_ip_address_container)
    (Ip_address_container_compact.inter t1.dst_ip_address_container t2.dst_ip_address_container)

let union t1 t2 =
  new_t
    (Ip_address_container_compact.union t1.src_ip_address_container t2.src_ip_address_container)
    (Ip_address_container_compact.union t1.dst_ip_address_container t2.dst_ip_address_container)
    
