
open Printf
    
open Admd_instantiations_for_classification

type t =
  {
    date : Core.Date.t;
    time : Core.Time.t;

    anomaly_type : Mawilab_anomaly_type.t;
    base_value : Admd.Instantiation.Base_value.t;
    anomaly_signature : Anomaly_signature.t;

    initial_description_string : string;
    detailed_metrics_string : string;

    anomaly_metric : Anomaly_metric.t;
    network_traffic_attributes : Network_traffic_attributes.t;
    network_traffic_values : Network_traffic_values.t;
    anomaly_raw_data : Anomaly_raw_data.t;

    slice_list :  Admd.Slice.t list;

    start_time : int;
    end_time : int;
  }

let new_t
    date
    time

    anomaly_type
    base_value

    initial_description_string
    detailed_metrics_string
    
    anomaly_metric
    network_traffic_attributes
    network_traffic_values
    anomaly_raw_data
    anomaly_signature
    

    slice_list

    start_time
    end_time
  =
  {
    date;
    time;

    anomaly_type;
    base_value;

    initial_description_string;
    detailed_metrics_string;
    
    anomaly_metric;
    network_traffic_attributes;
    network_traffic_values;
    anomaly_raw_data;
    anomaly_signature;

    slice_list;

    start_time;
    end_time;
  }

let to_xml_anomaly_simple_description
    indice
    t
  =
  let value =
    t.base_value
    ^ ","
    ^ (Anomaly_signature.to_short_name t.anomaly_signature)
  in

  let description =
    Admd.Instantiation.Base_description.of_string
      t.initial_description_string
  in

  let description_option = Some (description) in

  let anomaly =
    Mawilab_admd_classic.Anomaly.new_t
      indice

      (* t.date *)
      (* t.time *)

      t.anomaly_type
      value
      description_option

      t.slice_list

      t.start_time
      t.end_time
  in

  anomaly

let to_xml_anomaly_full_description
    indice
    t
  =
  let value =
    t.base_value
    ^ ","
    ^ (Anomaly_signature.to_short_name t.anomaly_signature)
  in

  let description =
    Xml_classification_description.new_t
      t.initial_description_string
      t.detailed_metrics_string

      t.anomaly_metric
      t.network_traffic_attributes
      t.network_traffic_values
  in

  let description_option = Some (description) in  

  let anomaly =
    Mawilab_admd_xml.Anomaly.new_t
      indice

      (* t.date *)
      (* t.time *)

      t.anomaly_type
      value
      description_option

      t.slice_list

      t.start_time
      t.end_time
  in

  anomaly
    

let to_binary_anomaly
  indice
  t
  =
  let value =
    t.base_value
    ^ ","
    ^ (Anomaly_signature.to_short_name t.anomaly_signature)
  in

  
  let mawilab_description_for_binary_classification =
    Binary_classification_description.new_t
      t.anomaly_metric
      t.network_traffic_attributes
      t.network_traffic_values

      t.anomaly_raw_data.Anomaly_raw_data.anomaly_ip_address
      t.anomaly_raw_data.Anomaly_raw_data.anomaly_port
  in

  let anomaly =
    Admd_mawilab_type_base_value_binary_description.Anomaly.new_t
      indice

      (* t.date *)
      (* t.time *)

      t.anomaly_type
      value
      (Some mawilab_description_for_binary_classification)

      t.slice_list

      t.start_time
      t.end_time
  in

  anomaly
    
