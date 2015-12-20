
open Printf
    
module L = BatList
  
(* open Mawilab_admd_instantiations *)

(* open Admd_mawilab_xml_classification_instantiation *)
(* open Admd_mawilab_binary_classification_instantiation *)

open Admd_instantiations_for_classification

open Day_anomaly_container_instantiation
    
let debug_enabled = ref false

let set_debug bool = debug_enabled := bool

let debug fmt =
  Printf.kprintf
    (
      if !debug_enabled then
        (fun s -> Format.printf "[Anomaly_data_container]: %s@." s)
      else
        ignore
    )
    fmt

type t =
  {
    basename_wo_extension : string;

    (* date : Core.Date.t; *)
    (* time : Core.Time.t; *)

    algorithm_option :  Admd.Algorithm.t option;
    analysis_option :  Admd.Analysis.t option;
    dataset_option :  Admd.Dataset.t option;

    l : Anomaly_data.t list;
  }

let new_t
    basename_wo_extension

    (* date *)
    (* time *)

    algorithm_option
    analysis_option
    dataset_option

    l
  =
  {
    basename_wo_extension;

    (* date; *)
    (* time; *)

    algorithm_option;
    analysis_option;
    dataset_option;

    l;
  }  

let of_data
    filepath

    (* date *)
    (* time *)

    algorithm_option
    analysis_option
    dataset_option

    l
  =
  let basename = Filename.basename filepath in

  let basename_wo_extension =
    try
      Filename.chop_extension basename
    with
    | Invalid_argument string ->
      basename
  in

  (* let basename_wo_extension = basename_wo_extension ^ "_exp" in *)

  new_t
    basename_wo_extension

    (* date *)
    (* time *)

    algorithm_option
    analysis_option
    dataset_option

    l
(* let of_list *)
(*     basename *)

(*     date *)
(*     time *)

(*     algorithm_option *)
(*     analysis_option *)
(*     dataset_option *)

(*     l *)
(*   = *)
(*   new_t *)
(*     basename *)

(*     date *)
(*     time *)

(*     algorithm_option *)
(*     analysis_option *)
(*     dataset_option *)

(*     l *)

(* let map_to_anomaly *)
(*     (\* ( *\) *)
(*     (\*   indice, *\) *)
(*     (\*   date, *\) *)
(*     (\*   time, *\) *)

(*     (\*   anomaly_type, *\) *)
(*     (\*   base_value, *\) *)

(*     (\*   initial_description_string, *\) *)
(*     (\*   detailed_metrics_string, *\) *)

(*     (\*   anomaly_metric, *\) *)
(*     (\*   network_traffic_attributes, *\) *)
(*     (\*   network_traffic_values, *\) *)
(*     (\*   (\\* anomaly_signature, *\\) *\) *)

(*     (\*   slice_list, *\) *)

(*     (\*   start_time, *\) *)
(*     (\*   end_time *\) *)
(*     (\* ) *\) *)
(*     anomaly_data *)
(*   = *)
(*   let value = *)
(*     anomaly_data.Anomaly_data.base_value *)
(*     ^ "," *)
(*     ^ (Anomaly_signature.to_string *)
(*          To_string_mode.Command *)
(*          anomaly_data.Anomaly_data.anomaly_signature *)
(*       ) *)
(*   in *)

(*   let description = *)
(*     Xml_classification_description.new_t *)
(*       initial_description_string *)
(*       detailed_metrics_string *)
(*       anomaly_metric *)
(*       network_traffic_attributes *)
(*       network_traffic_values *)

(*   (\* anomaly_raw_data.Anomaly_raw_data.anomaly_ip_address *\) *)
(*   (\* anomaly_raw_data.Anomaly_raw_data.anomaly_port *\) *)
(*   in *)

(*   let description_option = Some (description) in *)

(*   let anomaly = *)
(*     Admd_mawilab_type_base_value_xml_description.Anomaly.new_t *)
(*       indice *)

(*       date *)
(*       time *)

(*       anomaly_type *)
(*       value *)
(*       description_option *)

(*       slice_list *)

(*       start_time *)
(*       end_time *)
(*   in *)

(*   anomaly *)

let export_xml
    export_traffic_information

    t
  =
  (
    debug "export_xml: call";

    let filename =
      t.basename_wo_extension ^ "_cl.xml"
    in

    debug
      "export_xml: filename: %s"
      filename
    ;

    match export_traffic_information with
    | false ->
      (
        let anomaly_list =
          L.mapi
            Anomaly_data.to_xml_anomaly_simple_description
            t.l
        in

        let mawilab_anomaly_mod_container =
          (* Mawilab_base__for_xml_classification_admd.Anomaly_container.new_t *)
          Mawilab_admd_classic.Anomaly_container.new_t
            anomaly_list
        in

        let file =
          (* Mawilab_base__for_xml_classification_admd.File.new_t *)
          Mawilab_admd_classic.File.new_t
            filename

            (* t.date *)
            (* t.time *)

            t.algorithm_option
            t.analysis_option
            t.dataset_option

            mawilab_anomaly_mod_container
        in

        (* Mawilab_base__for_xml_classification_admd.File.to_filename *)
        Mawilab_admd_classic.File.to_filename
          file;
      )
    | true ->
      (
        let anomaly_list =
          L.mapi
            Anomaly_data.to_xml_anomaly_full_description
            t.l
        in

        let mawilab_anomaly_mod_container =
          (* Admd_mawilab_type_base_value_xml_description.Anomaly_container.new_t *)
          Mawilab_admd_xml.Anomaly_container.new_t
            anomaly_list
        in

        let attributes_metadata_string =
          Sexplib.Sexp.to_string
            (Feature_name_container.sexp_of_t
               (Network_traffic_attributes.generate_feature_name_container
                  ()
               )
            )
        in

        let analysis =
          match t.analysis_option with
          | None ->
             Admd.Analysis.new_t
              attributes_metadata_string
              ""
              ""
              ""
          | Some analysis ->
             Admd.Analysis.new_t
              (analysis. Admd.Analysis.description
               ^ "--" ^ attributes_metadata_string)
              ""
              ""
              ""
        in

        let file =
          (* Admd_mawilab_type_base_value_xml_description.File.new_t *)
          Mawilab_admd_xml.File.new_t
            filename

            (* t.date *)
            (* t.time *)

            t.algorithm_option
            (Some analysis)
            t.dataset_option

            mawilab_anomaly_mod_container
        in

        (* Admd_mawilab_type_base_value_xml_description.File.to_filename *)
        Mawilab_admd_xml.File.to_filename
          file;
      );

      debug "export_xml: end";
  )

let export_binary
    t
  =
  let anomaly_l =
    L.mapi
      Anomaly_data.to_binary_anomaly
      t.l
  in

  let day_anomaly_container =
    Day_anomaly_container_classification.new_t
      (* t.time *)

      (Admd_mawilab_type_classification_value_binary_description.Anomaly_container.new_t
         anomaly_l
      )
  in

  (* let prefix = *)
  (*   try *)
  (*     Filename.chop_extension t.basename_wo_extension *)
  (*   with *)
  (*   | Invalid_argument string -> *)
  (*     t.basename *)
  (* in *)

  Day_anomaly_container_classification.to_file_prefix
    day_anomaly_container
    t.basename_wo_extension
  ;
