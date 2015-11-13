
open Printf

module A = BatArray
module S = BatString
module HT = BatHashtbl
module L = BatList

open Admd.Instantiation

let debug_enabled = ref true

let set_debug bool = debug_enabled := bool

let debug fmt =
  Printf.kprintf
    (
      if !debug_enabled then
        (fun s -> Format.printf "[Filter_criteria_extractor]: %s@." s)
      else
        ignore
    )
    fmt

let extract
    anomaly_container
  =  
  debug "process: call";

  let filter_criteria_list_list_list =
    L.map
      (fun anomaly ->
         L.map
           (fun slice ->
              L.map
                (fun filter ->
                   filter. Admd.Filter.filter_criteria_list
                )
                slice. Admd.Slice.filter_list
           )
           anomaly.Base.Anomaly.slice_list
      )
      anomaly_container.Base.Anomaly_container.anomaly_list
  in

  let filter_criteria_list =
    L.flatten
      (L.flatten
         (L.flatten
            filter_criteria_list_list_list
         )
      )
  in

  let filter_criteria_list_sorted_unique =
    L.sort_unique 
      Admd.Filter_criteria.compare 
      filter_criteria_list
  in

  debug 
    "process: filter_criteria reduce from %d to %d"
    (L.length filter_criteria_list)
    (L.length filter_criteria_list_sorted_unique)
  ;

  debug "process: end";

  filter_criteria_list_sorted_unique
