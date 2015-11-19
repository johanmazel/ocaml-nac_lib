
open Printf
    
let run
    ?default_hour_minute_second
    time_format_string
    file_path
  =  
  let file_name = Filename.basename file_path in

  (* let file_name_wo_extension = Filename.chop_extension file_name in *)
  let file_name_wo_extension = file_name in

  let year, month, day, hour, minute, second =
    try
      match default_hour_minute_second with
      | None ->
        Scanf.sscanf
          file_name_wo_extension
          (Scanf.format_from_string time_format_string "%d%d%d%d%d%d%s")
          (* "%4d%2d%2d%s" *)
          (fun year month day hour minute second (string : string) (test : int list) -> (year, month, day, hour, minute, second))
          []
      | Some (hour, minute, second) ->
        (* File_managment_debug.debug *)
        (*   "File_paths_and_names_functor: get_time: default values: hour: %d ; minute: %d second: %d" *)
        (*   hour *)
        (*   minute *)
        (*   secon; *)
        let year, month, day =
          Scanf.sscanf
            file_name_wo_extension
            (Scanf.format_from_string time_format_string "%d%d%d%s")
            (* "%4d%2d%2d%s" *)
            (fun year month day (string : string) (test : int list) -> (year, month, day))
            []
        in
        year, month, day, hour, minute, second
    with
    | Scanf.Scan_failure string ->
      print_endline
        (sprintf
           "Extract_time_from_filename: get_time: \"%s\" does not match time format \"%s\":\n%s\n%s"
           file_name_wo_extension
           time_format_string
           string
           file_path
        );
      assert(false)
    | Failure string ->
      print_endline
        (sprintf
           "Extract_time_from_filename: get_time: error converting a number in \"%s\" from time format \"%s\":\n%s\n%s"
           file_name_wo_extension
           time_format_string
           string
           file_path
        );
      assert(false)
    | End_of_file ->
      print_endline
        (sprintf
           "Extract_time_from_filename: get_time: unexpected end-of-file in \"%s\" from time format \"%s\":\n%s"
           file_name_wo_extension
           time_format_string
           file_path
        );
      assert(false)
    | Invalid_argument string ->
      print_endline
        (sprintf
           "Extract_time_from_filename: get_time: time format \"%s\" is invalid:\n%s\n%s"
           time_format_string
           string
           file_path
        );
      assert(false)
  in

  (* year, month, day, hour, minute, second  *)

  let date =
    Core.Date.create_exn
      year
      (Core.Month.of_int_exn month)
      day
  in

  let ofday =
    Core.Ofday.create
      ~hr: hour
      ~min: minute
      ~sec: second
      ()
  in

  let time =
    Core.Time.of_date_ofday
      ~zone: Core.Zone.local
      date
      ofday
  in

  time
