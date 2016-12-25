
open Printf

open Sexplib.Std
open Bin_prot.Std

type t =
  {
    anomaly_metric : Anomaly_metric.t;
    network_traffic_attributes : Network_traffic_attributes.t;
    network_traffic_values : Network_traffic_values.t;
    anomaly_ip_address : Anomaly_ip_address.t;
    anomaly_port : Anomaly_port.t;
  }
[@@deriving compare, sexp, bin_io]

let new_t
    anomaly_metric
    network_traffic_attributes
    network_traffic_values
    anomaly_ip_address
    anomaly_port
    =
  {
    anomaly_metric;
    network_traffic_attributes;
    network_traffic_values;
    anomaly_ip_address;
    anomaly_port;
  }

let of_string
    string
  =
    failwith "DO NOT USE"
  
let to_string
    (* to_string_mode *)
    t
    =
    failwith "DO NOT USE"
      
(* let to_string_initial *)
(*     t *)
(*     = *)
(*   sprintf *)
(*     "%s" *)
(*     t.initial_description_string *)
