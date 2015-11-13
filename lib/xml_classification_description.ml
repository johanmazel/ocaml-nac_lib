
open Printf

(* module A = BatArray *)
module S = BatString
(* module HT = BatHashtbl *)
module L = BatList
  
open Sexplib.Std
open Bin_prot.Std

type t =
  {
    initial_description_string : string;
    detailed_metrics_string : string;

    anomaly_metric : Anomaly_metric.t;
    network_traffic_attributes : Network_traffic_attributes.t;
    network_traffic_values : Network_traffic_values.t;
  }
with compare, sexp, bin_io

let new_t
    initial_description_string
    detailed_metrics_string

    anomaly_metric
    network_traffic_attributes
    network_traffic_values
  =
  {
    initial_description_string;
    detailed_metrics_string;

    anomaly_metric;
    network_traffic_attributes;
    network_traffic_values;
  }

let of_string
    string
  =
  let string_list = Str.split (Str.regexp "\n--\n") string in
  let list_length = L.length string_list in

  if list_length <> 7 then
    (
      print_endline
        (sprintf
           "Mawilab_mod_anomaly_description: of_string: splitted description list length %d <> 5:\n%s"
           list_length
           string
        );
      assert(false)
    );

  let initial_description_string_from_file,
      detailed_metrics_string_from_file,

      anomaly_metric_string_from_file,

      network_traffic_attributes_string_from_file,
      network_traffic_values_string_from_file
    =
    S.trim (L.nth string_list 0),
    S.trim (L.nth string_list 1),
    S.trim (L.nth string_list 2),

    S.trim (L.nth string_list 5),
    S.trim (L.nth string_list 6)
  in

  let anomaly_metric =
    Anomaly_metric.t_of_sexp
      (Sexplib.Sexp.of_string
         anomaly_metric_string_from_file
      )
  in

  let network_traffic_attributes_string_from_file_fixed =
    Str.replace_first
      (Str.regexp "\n")
      ""
      network_traffic_attributes_string_from_file
  in

  let network_traffic_attributes =
    try
      Network_traffic_attributes.t_of_sexp
        (Sexplib.Sexp.of_string
           network_traffic_attributes_string_from_file_fixed
        )
    with
    | Failure message ->
      print_endline
        (sprintf
           "Mawilab_mod_anomaly_description: of_string: problem extracting network_attributes of string (\"%s\"):\n\"%s\"\nfixed:\"%s\""
           string
           network_traffic_attributes_string_from_file
           network_traffic_attributes_string_from_file_fixed
        );
      assert(false)
  in

  let network_traffic_values_string_from_file_fixed =
    Str.replace_first
      (Str.regexp "\n")
      ""
      network_traffic_values_string_from_file
  in

  let network_traffic_values =
    try
      Network_traffic_values.t_of_sexp
        (Sexplib.Sexp.of_string
           network_traffic_values_string_from_file_fixed
        )
    with
    | Failure message ->
      print_endline
        (sprintf
           "Mawilab_mod_simple_anomaly_description: of_string: problem extracting network_attributes of string (\"%s\"):\n\"%s\"\n\"%s\""
           message
           network_traffic_attributes_string_from_file
           network_traffic_attributes_string_from_file_fixed
        );
      assert(false)
  in

  new_t
    initial_description_string_from_file
    detailed_metrics_string_from_file
    anomaly_metric

    network_traffic_attributes
    network_traffic_values
    
let to_string
    t
  =
  sprintf
    "%s\n--\n%s\n--\n%s\n--\n%s\n--\n%s\n--\n%s\n--\n%s"
    t.initial_description_string
    t.detailed_metrics_string
    (Sexplib.Sexp.to_string (Anomaly_metric.sexp_of_t t.anomaly_metric))
    (Network_traffic_attributes.to_string t.network_traffic_attributes)
    (Network_traffic_values.to_string t.network_traffic_values)
    (Sexplib.Sexp.to_string (Network_traffic_attributes.sexp_of_t t.network_traffic_attributes))
    (Sexplib.Sexp.to_string (Network_traffic_values.sexp_of_t t.network_traffic_values))
