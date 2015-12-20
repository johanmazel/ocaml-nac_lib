
open Printf

module A = BatArray
module S = BatString
module HT = BatHashtbl
module L = BatList

open Set_ext_instantiations

open Admd_instantiations_for_classification
    
let debug_enabled = ref false

let set_debug bool = debug_enabled := bool

let debug fmt =
  Printf.kprintf
    (
      if !debug_enabled then
        (fun s -> Format.printf "[]: %s@." s)
      else
        ignore
    )
    fmt

type t =
  {
    (* time : Core.Time.t; *)
    
    (* anomaly_container : Admd_mawilab_type_base_value_binary_description.Anomaly_container.t; *)
    anomaly_container : Admd_mawilab_type_classification_value_binary_description.Anomaly_container.t;
  }
  with compare, bin_io

let new_t
    (* time *)

    anomaly_container
  =
  {
    (* time; *)

    anomaly_container;
  }

let generate_filename
    prefix
  =
  prefix
  ^
  "_anomaly_data.bin"

let to_file_prefix
    t 
    ?directory: (directory = "")
    prefix
  =
  (
    let filename = generate_filename prefix in

    let filename =
      (match directory with
       | "" -> ""
       | _ -> directory ^ "/"
      )
      ^
      filename
    in

    Bin_prot_marshal_manager.write_to_file
      bin_writer_t
      t
      filename
    ;
  )

let of_file_prefix prefix =
  (
    let filename = generate_filename prefix in

    let t =
      Bin_prot_marshal_manager.import_from_file
        bin_reader_t
        filename
    in

    t
  )
  
let of_file filename =
  (
    let t =
      Bin_prot_marshal_manager.import_from_file
        bin_reader_t
        filename
    in

    t
  )
