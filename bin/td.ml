open Tood

exception ConfigError of string

let _version = "v0.0"

(* Config type and config file handling *)

type config = {
    todo_path      : string
  ; done_path      : string
  ; sync_cmd       : string option
  ; date_fmt       : string
  ; date_fmt_parse : string option
  ; entry_fmt_flat : string
  ; entry_fmt_tree : string
}

(* Config format documentation
 * date_fmt  : %d -> day, %m -> month, %y -> year
 * entry_fmt : %i -> index, %I -> index left, %J -> index right,
 *             %r -> priority, %t -> text,
 *             %p -> project without tag, %P -> project with tag
 *             %c -> context without tag, %C -> context with tag
 *             %d -> due date without tag, %D -> due date with tag
 **)

let default_config = {
    todo_path      = "./todo.tood"
  ; done_path      = "./done.tood"
  ; sync_cmd       = None
  ; date_fmt       = "%y-%m-%y"
  ; date_fmt_parse = None
  ; entry_fmt_flat = "%J %r %t %P_%C_%D"
  ; entry_fmt_tree = "%J %r %t %C_%D"
}

let default_config_path = "~/.config/td/conf"

let add_config_option config (key, value) = match key with
  | "todo_path"      -> { config with todo_path = value }
  | "done_path"      -> { config with done_path = value }
  | "sync_cmd"       -> { config with sync_cmd = Some value }
  | "date_fmt"       -> { config with date_fmt  = value }
  | "date_fmt_parse" -> { config with date_fmt_parse = Some value }
  | "entry_fmt_flat" -> { config with entry_fmt_flat = value }
  | "entry_fmt_tree" -> { config with entry_fmt_tree = value }
  | _ -> raise (ConfigError ("invalid key: " ^ key))

let resolve_path path =
  match Sys.getenv "HOME" with
  | None -> path
  | Some home_dir -> 
    match String.is_substring_at path ~pos:0 ~substring:"~" with
    | false -> path
    | true  -> String.substr_replace_first path ~pattern:"~" ~with_:home_dir

let parse_config_file path = let open Tood.Parser in
  let path   = resolve_path path in
  let key    = symbol <* ws <* char ':' in
  let value  = take_all >>| String.strip in
  let t2 a b = Some (a, b) in
  let parser = lift2 t2 key value <|> only ws *> return None in
  let f line = match parse_string parser line with
    | Ok tup  -> tup
    | Error _ -> raise (ConfigError ("cannot parse line: " ^ line))
  in
  try
    List.filter_map ~f (In_channel.read_lines path)
    |> List.fold ~init:default_config ~f:add_config_option
  with
   Sys_error _ -> 
     prerr_endline
       ("Config file '" ^ path ^ "' cannot be opened. Using fallback options.");
     default_config


(* High level todo file operations *)

let parse_todo_file ?(warn=false) path =
  try
  List.map ~f:Entry.of_string_exn (In_channel.read_lines (resolve_path path))
  with
  _ -> match warn with
  | true  -> prerr_endline ("Cannot open file '" ^ path ^ "'"); []
  | false -> []

let write_todo_file entries path =
  List.map ~f:Entry.to_string entries
  |> Out_channel.write_lines (resolve_path path)

let append_todo_file entries path =
  let str =
    List.map ~f:Entry.to_string entries |> String.concat ~sep:"\n" 
  in
  let f oc = Out_channel.output_string oc (str ^ "\n") in
  Out_channel.with_file (resolve_path path) ~append:true ~f


(* Formatting entry lists for printing purposes *)

let layout_entries config layout indexed_entries =
  match Entry.max_index indexed_entries with
  | None -> "< no entries to print >"
  | Some max_index ->
    match layout with
    | `Flat ->
      let fmt = Entry.fmt config.entry_fmt_flat in
      let fmt_date = Date.fmt config.date_fmt in
      let f = Entry.format_index ~fmt ~fmt_date ~max_index in
      List.map ~f indexed_entries |> String.concat ~sep:"\n"
    | `Tree -> raise (Failure "tree layout not yet implemented")

let sort_entries sort indexed_entries = match sort with
  | `None -> indexed_entries


let prompt_confirmation msg default =
  let msg = msg ^ (if default then " [Y/n]" else " [N/y]") in
  print_endline msg;
  let rec read_answer () = begin
    match In_channel.(input_line stdin) |> Option.map ~f:String.lowercase with
    | None -> default
    | Some "y"  -> true
    | Some "n"  -> false
    | _ -> print_endline "You must type 'y' or 'n'"; read_answer ()
  end
  in read_answer ()

let get_fmt_date config = match config.date_fmt_parse with
  | None     -> Date.fmt config.date_fmt
  | Some str -> Date.fmt str


(* td commands *)

let td_add config str =
  let fmt_date = get_fmt_date config in
  match Entry.of_string_relaxed ~fmt_date str with
  | Error err -> prerr_endline err
  | Ok entry  -> append_todo_file [entry] config.todo_path

let move config verbose noprompt sel msg from_path to_path =
  let entries = Entry.index (parse_todo_file from_path) in
  let selected, unselected =
    Select.split_indexed sel entries
  in
  match entries with
  | [] -> print_endline "< no entries selected >"
  | entries ->
    if verbose then begin
      print_endline "Selected entries:"; 
      selected |> layout_entries config `Flat |> print_endline;
    end;
    let proceed =
      match noprompt || List.length selected <= 1 with
      | true  -> true
      | false ->
        let l = List.length entries |> Int.to_string in
        prompt_confirmation 
          (msg l)
          true
    in
    match proceed with
    | false -> print_endline "Aborted."
    | true  ->
      write_todo_file (Entry.deindex unselected) from_path;
      match to_path with
      | None -> ()
      | Some to_path ->
        append_todo_file (Entry.deindex selected) to_path

let td_do config verbose noprompt filter =
  let fmt_date = get_fmt_date config in
  match Select.of_string ~fmt_date filter with
  | Error err -> prerr_endline err
  | Ok sel -> 
    let msg l = 
      "You are about to mark " ^ l ^ " entries as done. Proceed?"
    in
    move 
      config verbose noprompt sel msg
      config.todo_path (Some config.done_path)

let td_undo config verbose noprompt filter =
  let fmt_date = get_fmt_date config in
  match Select.of_string ~fmt_date filter with
  | Error err -> prerr_endline err
  | Ok sel -> 
    let msg l =
      "You are about to mark " ^ l ^ " done entries as undone. Proceed?"
    in
    move
      config verbose noprompt sel msg
      config.done_path (Some config.todo_path)

let td_rm config verbose noprompt source filter =
  let fmt_date = get_fmt_date config in
  match Select.of_string ~fmt_date filter with
  | Error err -> prerr_endline err
  | Ok sel -> 
    let msg, path = match source with
    | `Todo -> 
      let msg l =
        "You are about to delete " ^ l ^ " undone entries. Proceed?"
      in
      msg, config.todo_path
    | `Done ->
      let msg l =
        "You are about to delete " ^ l ^ " done entries. Proceed?"
      in
      msg, config.done_path
    | `All ->
      raise (Failure "NOT IMPLEMENTED YET")
    in
    move config verbose noprompt sel msg path None

let td_ls config source order layout filter =
  let fmt_date = get_fmt_date config in
  match Select.of_string ~fmt_date filter with
  | Error err -> prerr_endline err
  | Ok sel -> 
    let entries = match source with
      | `Todo -> parse_todo_file config.todo_path
      | `Done -> parse_todo_file config.done_path
      | `All  -> raise (Failure "NOT IMPLEMENTED YET")
        (*parse_todo_file config.todo_path @ parse_todo_file config.done_path *)
    in
    Entry.index entries
    |> Select.filter_indexed sel
    |> sort_entries order
    |> layout_entries config layout
    |> print_endline


(* command line parsing *)
open Cmdliner

let config_t =
  let doc = "Path to the td configuration file." in
  let path =
    Arg.(value & opt non_dir_file default_config_path & info ["c"; "config"]
    ~doc ~docv:"CONFIG-FILE")
  in
  Term.(const parse_config_file $ path)

let add_cmd =
  let doc = "add new entries" in
  let man = [
    `S Manpage.s_description;
    `P "Parses the provided command line arguments as tood entry and adds it to
    the todo.tood file specified in the configuration."
  ] in
  let entry_t =
    Arg.(non_empty & pos_all string [] & info [] ~docv:"TODO")
    |> Term.(app (const (String.concat ~sep:" ")))
  in
  Term.(const td_add $ config_t $ entry_t),
  Term.info "add" ~doc ~sdocs:Manpage.s_common_options ~man

let do_cmd =
  let doc = "Filter expression used to select entries." in
  let filter_t =
    Arg.(non_empty & pos_all string [] & info [] ~doc ~docv:"FILTER")
    |> Term.(app (const (String.concat ~sep:" ")))
  in
  let doc = "List selected entries." in
  let verbose_t =
    Arg.(value & flag & info ["v"; "verbose"] ~doc)
  in
  let doc = 
    "Suppress asking for confirmation when more than 1 entry is selected."
  in
  let noprompt_t =
    Arg.(value & flag & info ["n"; "noprompt"] ~doc)
  in
  let doc = "mark entries as done" in
  let man = [
    `S Manpage.s_description;
    `P "Parses the positional command line arguments as filter and moves the
    selected entries from the todo.tood to the done.tood file specified in the
    configuration. Prompts for confirmation if more than one entry is selected
    (see -n, --noprompt)."
  ] in
  Term.(const td_do $ config_t $ verbose_t $ noprompt_t $ filter_t),
  Term.info "do" ~doc ~sdocs:Manpage.s_common_options ~man

let undo_cmd =
  let doc = "Filter expression used to select entries." in
  let filter_t =
    Arg.(non_empty & pos_all string [] & info [] ~doc ~docv:"FILTER")
    |> Term.(app (const (String.concat ~sep:" ")))
  in
  let doc = "List selected entries." in
  let verbose_t =
    Arg.(value & flag & info ["v"; "verbose"] ~doc)
  in
  let doc = 
    "Suppress asking for confirmation when more than 1 entry is selected."
  in
  let noprompt_t =
    Arg.(value & flag & info ["n"; "noprompt"] ~doc)
  in
  let doc = "mark entries as undone" in
  let man = [
    `S Manpage.s_description;
    `P "Parses the positional command line arguments as filter and moves the
    selected entries from the done.tood to the todo.tood file specified in the
    configuration. Prompts for confirmation if more than one entry is selected
    (see -n, --noprompt)."
  ] in
  Term.(const td_undo $ config_t $ verbose_t $ noprompt_t $ filter_t),
  Term.info "undo" ~doc ~sdocs:Manpage.s_common_options ~man

let rm_cmd =
  let doc = "Filter expression used to select entries." in
  let filter_t =
    Arg.(non_empty & pos_all string [] & info [] ~doc ~docv:"FILTER")
    |> Term.(app (const (String.concat ~sep:" ")))
  in
  let doc = "List selected entries." in
  let verbose_t =
    Arg.(value & flag & info ["v"; "verbose"] ~doc)
  in
  let doc = 
    "Suppress asking for confirmation when more than 1 entry is selected."
  in
  let noprompt_t =
    Arg.(value & flag & info ["n"; "noprompt"] ~doc)
  in
  let source_t =
    let doc   = "Remove entries from the todo.txt file (default)." in
    let todo' = `Todo, Arg.info ["t"; "todo"] ~doc in
    let doc   = "Remove entries from the done.txt file." in
    let done' = `Done, Arg.info ["d"; "done"] ~doc in
    let doc   = "Remove entries from both the todo.txt and done.txt files." in
    let all'  = `All, Arg.info ["a"; "all"] ~doc in
    Arg.(value & vflag `Todo [todo'; done'; all'])
  in
  let doc = "remove entries" in
  let man = [
    `S Manpage.s_description;
    `P "Parses the positional command line arguments as filter and removes the
    selected entries from the todo.tood or done.tood files specified in the
    configuration. Prompts for confirmation if more than one entry is selected
    (see -n, --noprompt)."
  ] in
  Term.(const td_rm $ config_t $ verbose_t $ noprompt_t $ source_t $ filter_t),
  Term.info "rm" ~doc ~sdocs:Manpage.s_common_options ~man


let ls_cmd =
  let doc = "pretty print entries" in
  let man = [
    `S Manpage.s_description;
    `P "List selected entries from the todo.tood or done.tood files with custom
    formatting and style."
  ] in
  let filter_t =
    Arg.(value & pos_all string [] & info [] ~docv:"FILTER")
    |> Term.(app (const (String.concat ~sep:" ")))
  in
  let source_t =
    let doc   = "List entries from the todo.txt file (default)." in
    let todo' = `Todo, Arg.info ["t"; "todo"] ~doc in
    let doc   = "List entries from the done.txt file." in
    let done' = `Done, Arg.info ["d"; "done"] ~doc in
    let doc   = "List entries from both the todo.txt and done.txt files." in
    let all'  = `All, Arg.info ["a"; "all"] ~doc in
    Arg.(value & vflag `Todo [todo'; done'; all'])
  in
  let layout_t =
    let doc   = "List entries as flat list (default)." in
    let flat' = `Flat, Arg.info ["flat"] ~doc in
    let doc   = "List entries as project tree." in
    let tree' = `Tree, Arg.info ["tree"] ~doc in
    Arg.(value & vflag `Flat [flat'; tree'])
  in
  let order_t =
    let order = function
      | "none" -> `None
      | arg    -> raise (Invalid_argument arg)
    in
    let doc = "Ordering of the printed entries. Must be one of none, .... " in
    Arg.(value & opt string "none" & info ["o"; "order"] ~docv:"ORDER" ~doc)
    |> Term.(app (const order))
  in
  Term.(const td_ls $ config_t $ source_t $ order_t $ layout_t $ filter_t),
  Term.info "ls" ~doc ~sdocs:Manpage.s_common_options ~man

let default_cmd =
  let doc = "simple command line tool to manage todo lists" in
  let sdocs = Manpage.s_common_options in
  let exits = Term.default_exits in
  Term.(ret (const (fun _ -> `Help (`Pager, None)) $ config_t)),
  Term.info "td" ~version:_version ~doc ~sdocs ~exits

let commands = [add_cmd; ls_cmd; do_cmd; undo_cmd; rm_cmd]
let () = Term.(exit @@ eval_choice default_cmd commands)

