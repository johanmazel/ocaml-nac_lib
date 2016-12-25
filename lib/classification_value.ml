
open Printf

open Sexplib.Std
open Bin_prot.Std
       
module L = BatList

type t =
  {
    data : string;

    anomaly_signature : Anomaly_signature.t;
  }
[@@deriving compare, sexp, bin_io]

let new_t
    data
    anomaly_signature
  =
  {
    data;
    anomaly_signature;
  }
    
let of_string
    string
  =
  let list = Str.split (Str.regexp ",") string in
  let list_length = List.length list in

  if list_length < 2 then
    (
      failwith
        (sprintf
           "[Value]: of_string: invalid string: expected at least 2 elements separated by \",\" got %d: %s"
           list_length
           string
        );
    );

  let string_list_wo_last = L.rev (L.tl (L.rev list)) in
  let string = String.concat "," string_list_wo_last in

  let anomaly_signature_string = L.hd (L.rev list) in
  let anomaly_signature = Anomaly_signature.of_string_empty anomaly_signature_string in

  new_t
    string

    anomaly_signature

let to_string
    t
  =
  sprintf
    "%s,%s"
    t.data

    (* (Anomaly_signature.to_string To_string_mode.Command t.anomaly_signature) *)
    (Anomaly_signature.to_short_name t.anomaly_signature)
