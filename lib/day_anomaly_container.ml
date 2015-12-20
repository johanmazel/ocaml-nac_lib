
open Printf

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


module type Container = sig
  type t

  val compare : t -> t -> int

  val t_of_sexp : Sexplib.Sexp.t -> t
  val sexp_of_t : t -> Sexplib.Sexp.t

  val bin_size_t : t -> int
  val bin_write_t : Bin_prot.Common.buf -> pos:int -> t -> int
  val bin_read_t : Bin_prot.Common.buf -> pos_ref:Bin_prot.Common.pos_ref -> t
end

module Make (Container : Container) = struct


  type t =
    {
      container : Container.t;
    }
  with compare, bin_io

  let new_t
      container
    =
    {
      container;
    }

  let generate_filename
      prefix
    =
    prefix
    ^
    ".bin"

  let to_file_prefix
      t 
      ?directory: (directory = "")
      prefix
    =
    (
      print_endline (sprintf "[Day_anomaly_container]: to_file_prefix: prefix %s" prefix);
      
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

end
