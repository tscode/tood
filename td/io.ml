
let with_open_in f fname =
  let ic = open_in fname in
  match f ic with
  | res -> close_in ic; res
  | exception exn -> close_in ic; raise exn

let read_lines fname =
  let rec collect_lines acc ic = match input_line ic with
  | line -> collect_lines (line :: acc) ic
  | exception End_of_file -> acc
  in
  with_open_in (collect_lines []) fname |> List.rev

let write_lines ?(append=false) fname lines =
  let oc = match append with
  | false -> open_out fname
  | true -> open_out_gen [Open_append; Open_creat] 0o666 fname
  in
  let lines = List.map (fun l -> l ^"\n") lines in
  match List.iter (output_string oc) lines with
  | () -> close_out oc
  | exception exn -> close_out oc; raise exn

let write_all fname data =
  let oc = open_out fname in
  match output_string oc data with
  | () -> close_out oc
  | exception exn -> close_out oc; raise exn
