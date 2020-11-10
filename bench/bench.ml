open Tood
open Printf

let now = Unix.gettimeofday

let time name msg f =
  let t = now () in
  printf "%s:\n    %s...\n" name msg;
  let res = match f () with
  | res -> printf "    completed in %f seconds\n" (now () -. t); res
(*  | exception _ -> printf "    caught unexpected exception\n"; [] *)
  in
  flush_all ();
  res

let random_symbol_char _ =
  let rint = Random.int 26 in
  if Random.float 1. < 0.2 then Char.chr (65 + rint) else Char.chr (97 + rint)

let random_symbol _ =
  String.init (Random.int 15 + 1) random_symbol_char

let random_text _ =
  List.init (Random.int 4 + 1) random_symbol |> String.concat " "

let random_date _ =
  let day = Random.int 29 + 1 in
  let month = Random.int 11 + 1 in
  sprintf "2000-%d-%d" month day

let random_prio () =
  match Random.int 2 with | 0 -> "?" | 1 -> "-" | _ -> "!"

let random_project_tags _ =
  let tag _ =
    List.init (Random.int 2 + 1) random_symbol
    |> String.concat "/"
    |> (^) "+"
  in
  List.init (Random.int 5) tag |> String.concat " "

let random_context_tags _ =
  List.init (Random.int 5) random_symbol |> List.map ((^) "+") |> String.concat " "

let random_date_tags _ =
  List.init (Random.int 5) random_date |> List.map ((^) "+") |> String.concat " "

let random_entry _ =
  String.concat " "
  [ random_prio ()
  ; random_text ()
  ; random_project_tags ()
  ; random_context_tags ()
  ; random_date_tags () ]


let bench seed fname n =
  print_endline "benchmark started";
  let t = now () in
  Random.init seed;
  let entry_strings =
    let name = "gen_entry_strings" in
    let msg = sprintf "generating %n entry strings" n in
    time name msg (fun () -> List.init n random_entry)
  in
  let entries =
    let name = "parse_entry_strings" in
    let msg = sprintf "parsing %n entry strings" n in
    let parse_entry_strings () =
      List.map Entry.of_string_exn entry_strings |> Entry.index
    in
    time name msg parse_entry_strings
  in
  let n = List.length entries in
  let _ =
    let name = "filter_entries" in
    let msg = sprintf "applying filters to %n entries" n in
    let filter =
      Filter.of_string_exn
      "1..1000 or +haha or bl or (not j) or +2000-01-01..2000-03-15"
    in
    time name msg (fun () -> Filter.filter_indexed filter entries)
  in
  let _ =
    let name = "format_entries" in
    let msg = sprintf "formatting %n entries" n in
    time name msg (fun () -> List.map (Entry.format_indexed "%I %r %s %t") entries)
  in
  let _ =
    let name = "write_tood_file" in
    let msg = sprintf "formatting and writing %n entries to file %s" n fname in
    let write_tood_file () =
      let oc = open_out fname in
      let f (_, v) = output_string oc (Entry.to_string_strict v ^ "\n") in
      List.iter f entries;
      close_out oc; []
    in
    time name msg write_tood_file
  in
  let _entries =
    let name = "parse_tood_file" in
    let msg = sprintf "reading %n entries from file %s" n fname in
    let parse_tood_file () =
      let rec next_line ic acc = match input_line ic with
      | line -> next_line ic (Entry.of_string_exn line :: acc) 
      | exception End_of_file -> List.rev acc
      in
      let ic = open_in fname in
      let entries = next_line ic [] in
      close_in ic;
      entries
    in
    time name msg parse_tood_file
  in
  printf "benchmark completed in %f seconds\n" (now () -. t)

let seed = ref 1
let fname = ref (Filename.temp_file "todo" ".txt")
let n = ref 1000

let speclist = 
  [ ("-s", Arg.Set_int seed, ": initial seed for the benchmark")
  ; ("-f", Arg.Set_string fname, ": name of the temporary file")
  ; ("-n", Arg.Set_int n, ": number of entries for the benchmark") ]

let usage_msg = "Simple benchmark tool for the tood library."

(* parse the cmd arguments *)
let () = Arg.parse speclist (Fun.const ()) usage_msg

(* run the benchmark *)
let () = bench !seed !fname !n

