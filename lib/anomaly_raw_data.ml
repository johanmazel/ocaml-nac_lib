
open Printf

module A = BatArray
module L = BatList

open Sexplib.Std
open Bin_prot.Std

type t =
  {
    anomaly_ip_address : Anomaly_ip_address.t;
    anomaly_port : Anomaly_port.t;
  }
with compare, sexp, bin_io

let new_t
    anomaly_ip_address
    anomaly_port
    =
  {
    anomaly_ip_address;
    anomaly_port;
  }

let of_detailed_metrics
    detailed_metrics
    =
  let anomaly_ip_address = Anomaly_ip_address.of_detailed_metrics detailed_metrics in
  let anomaly_port = Anomaly_port.of_detailed_metrics detailed_metrics in
  new_t
    anomaly_ip_address
    anomaly_port
