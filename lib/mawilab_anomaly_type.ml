
open Printf

open Bin_prot.Std

type t =
| Anomalous
| Suspicious
| Notice
[@@deriving compare, sexp, bin_io]

let bin_size_t_ = bin_size_t
let bin_write_t_ = bin_write_t

(* let pouet (writer : Detector.bin_writer_t) = *)
(*   "" *)

let of_string
    string
    =
  match string with
  | "anomalous" -> Anomalous
  | "suspicious" -> Suspicious
  | "notice" -> Notice
  | _ -> failwith (sprintf "Mawilab_anomaly_type: from_string: invalid string input: \"%s\"" string)

(* TODO: figure out why space after anomalous and notice to equal length of suspicious... *)
(* let to_string to_string_mode t = *)
(* match to_string_mode with *)
(* | To_string_mode.Command ->        *)
(*     (match t with                  *)
(*       | Anomalous -> "anomalous"   *)
(*       | Suspicious -> "suspicious" *)
(*       | Notice -> "notice")        *)
(* | To_string_mode.Simple ->         *)
(*     (match t with                  *)
(*       | Anomalous -> "anomalous"   *)
(*       | Suspicious -> "suspicious" *)
(*       | Notice -> "notice")        *)
(* | To_string_mode.Normal ->         *)
(*     (match t with                  *)
(*       | Anomalous -> "Anomalous "  *)
(*       | Suspicious -> "Suspicious" *)
(*       | Notice -> "Notice")        *)
let to_string t =
  match t with
  | Anomalous -> "anomalous"
  | Suspicious -> "suspicious"
  | Notice -> "notice"

let to_mawilab_string
    t
    =
  to_string
    (* To_string_mode.Command *)
    t

let to_list () = [ Anomalous ; Suspicious ; Notice ]

(* let get_sqlite_column_name () = "mawilab_anomaly_type" *)

(* let to_sqlite_filter_string                                                *)
(*     t                                                                      *)
(* =                                                                          *)
(*   (get_sqlite_column_name ()) ^ "=" ^ (to_string To_string_mode.Command t) *)
