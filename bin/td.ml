open Tood

exception ConfigError of string
exception NotImplemented of string

let _version = "v0.0"


(* Config type and config option handling *)

type config = {
    todo_path      : string
  ; done_path      : string
  ; sync_cmd       : string option
  ; entry_fmt_flat : string
  ; entry_fmt_tree : string
  ; date_fmt       : Date.fmt
}

let default_config = {
    todo_path      = "./todo.tood"
  ; done_path      = "./done.tood"
  ; sync_cmd       = None
  ; entry_fmt_flat = "%J %r %t %P_%C_%D"
  ; entry_fmt_tree = "%J %r %t %C_%D"
  ; date_fmt       = Date.fmt_exn "%y-%m-%d"
}

let default_config_path = "~/.config/td/conf"

let add_config_option config (key, value) = match Symbol.to_string key with
  | "todo_path"      -> { config with todo_path = value }
  | "done_path"      -> { config with done_path = value }
  | "sync_cmd"       -> { config with sync_cmd  = Some value }
  | "date_fmt"       -> { config with date_fmt  = Date.fmt_exn value }
  | "entry_fmt_flat" -> { config with entry_fmt_flat = value }
  | "entry_fmt_tree" -> { config with entry_fmt_tree = value }
  | _ -> raise (ConfigError ("invalid key: " ^ Symbol.to_string key))


(* Logging *)

let log_info_str str = "< " ^ str ^ " >"
let log_warn_str str = "warning: " ^ str ^ "."
let log_err_str  str = "error: " ^ str ^ "."

let log_info str = log_info_str str |> print_endline
let log_warn str = log_warn_str str |> print_endline
let log_err str  = log_err_str str  |> prerr_endline


(* File operations *)

let resolve_path path =
  match Sys.getenv "HOME" with
  | None -> path
  | Some home_dir -> 
    match String.is_substring_at path ~pos:0 ~substring:"~" with
    | false -> path
    | true  -> String.substr_replace_first path ~pattern:"~" ~with_:home_dir

let parse_config_file path =
  let open Angstrom in
  let open Parser in
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
     log_info
       ("file '" ^ path ^ "' cannot be accessed, using fallback config options");
     default_config

let parse_todo_file ?(fail=false) path =
  try
    let lines = In_channel.read_lines (resolve_path path) in
    List.map ~f:Entry.of_string_exn lines
  with
  | Sys_error err as error -> begin match fail with
    | true  -> raise error
    | false -> log_info
      ("file '" ^ path ^ "' cannot be accessed");
      []
  end
  | error -> raise error

let write_todo_file path entries =
  List.map ~f:Entry.to_string entries
  |> Out_channel.write_lines (resolve_path path)

let append_todo_file entries path =
  let str =
    List.map ~f:Entry.to_string entries |> String.concat ~sep:"\n" 
  in
  let f oc = Out_channel.output_string oc (str ^ "\n") in
  Out_channel.with_file (resolve_path path) ~append:true ~f

(* Formatting entry lists for printing purposes *)

let is_done ~done_index (index, entry) =
  match done_index, Int.(done_index <= index) with
  | 0, _     -> false
  | _, value -> value

let layout_entry ?(style=None) ?(offset=0) ~fmt ~fmt_date ~max_index entry =
  (String.make offset ' ') ^ (Entry.format_index ~fmt_date ~max_index fmt entry)

let layout_entries config layout indexed_entries =
  match Entry.max_index indexed_entries with
  | None -> log_info_str "no entries to print"
  | Some max_index ->
    match layout with
    | `Flat ->
      let fmt = config.entry_fmt_flat in
      let fmt_date = config.date_fmt in
      let f = layout_entry ~fmt ~fmt_date ~max_index in
      List.map ~f indexed_entries |> String.concat ~sep:"\n"
    | `Tree ->
      let fmt = config.entry_fmt_tree in
      let fmt_date = config.date_fmt in
      let paths (_, entry) = match Entry.project_tags entry with
        | []       -> [[]]
        | projects -> projects
      in
      let tree = Tree.of_leaves ~paths indexed_entries in
      let fname path name = 
        let offset = Int.max 0 (List.length path - 1) in 
        String.make offset ' ' ^ Symbol.to_string name ^ "/"
      in
      let f path entry =
        let offset = Int.max 0 (List.length path - 1) in
        layout_entry ~offset ~fmt ~fmt_date ~max_index entry
      in
      Tree.collect ~fname ~f tree |> List.tl_exn |> String.concat ~sep:"\n"


let sort_entries sort indexed_entries = match sort with
  | `None -> indexed_entries

let prompt_confirmation msg default =
  let msg = msg ^ (if default then " Proceed? [Y/n] " else " Proceed? [N/y] ") in
  print_endline msg;
  let rec read_answer () = begin
    match In_channel.(input_line stdin) |> Option.map ~f:String.lowercase with
    | None | Some "" -> default
    | Some "y"  -> true
    | Some "n"  -> false
    | _ -> print_endline "You must type 'y' or 'n'"; read_answer ()
  end
  in read_answer ()

let list_entries_if_verbose config verbose selected = match verbose with
  | false -> ()
  | true  ->
    print_endline "Selected entries:"; 
    selected |> layout_entries config `Flat |> print_endline

(* td commands *)

let _td_add config entry_str =
  let fmt_date = config.date_fmt in
  let entry = Entry.of_string_exn ~fmt_date entry_str in
  append_todo_file [entry] config.todo_path

let move ?(f=fun x -> x) config verbose noprompt sel msg from_path to_path =
  let entries = Entry.index (parse_todo_file from_path) in
  let selected, unselected =
    Filter.split_indexed sel entries
  in
  match selected with
  | [] -> log_info "no entries selected"
  | _  ->
    let () = list_entries_if_verbose config verbose selected in
    let proceed =
      match noprompt || List.length selected <= 1 with
      | true  -> true
      | false ->
        let l = List.length selected |> Int.to_string in
        prompt_confirmation (msg l) true
    in
    match proceed with
    | false -> log_info "aborted"
    | true  ->
      write_todo_file from_path (Entry.deindex unselected);
      match to_path with
      | None -> ()
      | Some to_path ->
        let selected = List.map ~f:(fun (i,x) -> (i, f x)) selected in
        append_todo_file (Entry.deindex selected) to_path

let modify config verbose noprompt sel msg path mods =
  let entries = parse_todo_file path |> Entry.index in
  match Filter.filter_indexed sel entries with
  | []       -> log_info "no entries modified"
  | selected ->
    let () = list_entries_if_verbose config verbose selected in
    let proceed =
      match List.length selected, List.exists ~f:Mod.is_text mods with
      | 1, _ -> true
      | l, false -> if noprompt then true else prompt_confirmation (msg l) true
      | l, true  ->
          log_warn ("replacing text of " ^ Int.to_string l ^ " entries at once");
          if noprompt then true else prompt_confirmation (msg l) true
    in
    match proceed with
    | false -> log_info "aborted"
    | true  -> 
      entries
      |> Filter.map_indexed sel ~f:(Mod.apply_list mods)
      |> Entry.deindex
      |> write_todo_file path

let parse_mod_args fmt_date str =
  let open Angstrom in
  let open Parser in
  let p = lift2 Tuple2.create (filter ~fmt_date () <* ws) (mods ~fmt_date ()) in
  match parse_string p str with
  | Ok t    -> t
  | Error _ ->
    let msg = 
      "wrong mod syntax: '" ^ str ^ "' (should be 'filter mod1 [mod2 ...]')"
    in
    raise (ArgumentError msg)

let _td_mod config noprompt verbose source str =
  let sel, mods = parse_mod_args config.date_fmt str in
  let path, msg_frac = match source with
    | `Todo -> config.todo_path, " entries."
    | `Done -> config.done_path, " done entries."
    | `All -> raise (NotImplemented "flag --all not implemented yet - wait for version 0.2")
  in
  let msg l = ("You are about to modify " ^ Int.to_string l ^ msg_frac) in
  modify config verbose noprompt sel msg path mods

let _td_do config verbose noprompt failed filter =
  let fmt_date = config.date_fmt in
  let sel = Filter.of_string_exn ~fmt_date filter in
  let msg l = 
    "You are about to mark " ^ l ^ " entries as done."
  in
  let f = match failed with
    | true  -> Mod.(apply (of_string_exn "+failed"))
    | false -> fun entry -> entry
  in
  move ~f
    config verbose noprompt sel msg
    config.todo_path (Some config.done_path)

let _td_undo config verbose noprompt filter =
  let fmt_date = config.date_fmt in
  let sel = Filter.of_string_exn ~fmt_date filter in
  let msg l =
    "You are about to mark " ^ l ^ " done entries as undone."
  in
  move
    config verbose noprompt sel msg
    config.done_path (Some config.todo_path)

let _td_rm config verbose noprompt source filter =
  let fmt_date = config.date_fmt in
  let sel = Filter.of_string_exn ~fmt_date filter in
  let msg, path = match source with
  | `Todo -> 
    let msg l =
      "You are about to delete " ^ l ^ " entries."
    in
    msg, config.todo_path
  | `Done ->
    let msg l =
      "You are about to delete " ^ l ^ " done entries."
    in
    msg, config.done_path
  | `All ->
    raise (NotImplemented "Flag --all not implemented yet. Wait for version 0.2")
  in
  move config verbose noprompt sel msg path None

let _td_ls config source order layout filter =
  let fmt_date = config.date_fmt in
  let sel = Filter.of_string_exn ~fmt_date filter in
  let entries = match source with
    | `Todo -> parse_todo_file config.todo_path
    | `Done -> parse_todo_file config.done_path
    | `All  -> 
      parse_todo_file config.todo_path @ parse_todo_file config.done_path
    in
    Entry.index entries
    |> Filter.filter_indexed sel
    |> sort_entries order
    |> layout_entries config layout
    |> print_endline


let _td_sync config = 
  match config.sync_cmd with
  | None     -> log_info "no sync command configured"
  | Some cmd -> match Unix.system cmd with
    | Ok ()   -> log_info "synchronization successfull"
    | Error _ -> raise (ConfigError "sync command exited unsuccessfully")

let _td_touch config =
  let f file = () in
  Out_channel.with_file (resolve_path config.todo_path) ~append:true ~f;
  Out_channel.with_file (resolve_path config.done_path) ~append:true ~f

let wrap f = try f () with
  | Sys_error err 
  | ConfigError err
  | ArgumentError err 
  | NotImplemented err -> log_err err
  | error -> raise error

let td_add config entry_str =
  wrap (fun () -> _td_add config entry_str)

let td_do config verbose noprompt failed filter =
  wrap (fun () -> _td_do config verbose noprompt failed filter)

let td_undo config verbose noprompt filter =
  wrap (fun () -> _td_undo config verbose noprompt filter)

let td_rm config verbose noprompt source filter =
  wrap (fun () -> _td_rm config verbose noprompt source filter)

let td_ls config source order layout filter =
  wrap (fun () -> _td_ls config source order layout filter)

let td_sync config  = wrap (fun () -> _td_sync config)
let td_touch config = wrap (fun () -> _td_touch config)

let td_mod config noprompt verbose source filter_mod_str = 
  wrap (fun () -> _td_mod config noprompt verbose source filter_mod_str)

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
    the todo-file specified in the configuration."
  ] in
  let entry_t =
    Arg.(non_empty & pos_all string [] & info [] ~docv:"TODO")
    |> Term.(app (const (String.concat ~sep:" ")))
  in
  Term.(const td_add $ config_t $ entry_t),
  Term.info "add" ~doc ~sdocs:Manpage.s_common_options ~man


let mod_cmd =
  let doc = "List selected entries before prompting." in
  let verbose_t =
    Arg.(value & flag & info ["v"; "verbose"] ~doc)
  in
  let source_t =
    let doc   = "Modify entries in todo-file (default)." in
    let todo' = `Todo, Arg.info ["t"; "todo"] ~doc in
    let doc   = "Modify entries in done-file." in
    let done' = `Done, Arg.info ["d"; "done"] ~doc in
    let doc   = "Modify entries in both todo-file and done-file." in
    let all'  = `All, Arg.info ["a"; "all"] ~doc in
    Arg.(value & vflag `Todo [todo'; done'; all'])
  in
  let doc =
    "Suppress asking for confirmation when more than one entry is selected."
  in
  let noprompt_t =
    Arg.(value & flag & info ["n"; "noprompt"] ~doc)
  in
  let doc =
    "Filter to select the entries to be modified and (space separated) list of
    modifications."
  in
  let filter_mod_t =
    Arg.(non_empty & pos_all string [] & info [] ~doc ~docv:"FILTER MODS")
    |> Term.(app (const (String.concat ~sep:" ")))
  in
  let doc = "modify entries" in
  let man = [
    `S Manpage.s_description;
    `P "Modify an entry or a selection of entries. It is possible to modify
    the priority or the text of an entry, as well as to add or remove tags."
  ] in
  Term.(const td_mod $ config_t $ noprompt_t $ verbose_t $ source_t $ filter_mod_t),
  Term.info "mod" ~doc ~sdocs:Manpage.s_common_options ~man


let do_cmd =
  let doc = "Filter expression used to select entries." in
  let filter_t =
    Arg.(non_empty & pos_all string [] & info [] ~doc ~docv:"FILTER")
    |> Term.(app (const (String.concat ~sep:" ")))
  in
  let doc = "List selected entries before prompting." in
  let verbose_t =
    Arg.(value & flag & info ["v"; "verbose"] ~doc)
  in
  let doc = 
    "Suppress confirmation prompt if more than 1 entry is selected."
  in
  let noprompt_t =
    Arg.(value & flag & info ["n"; "noprompt"] ~doc)
  in
  let doc =
    "Add the tag +failed when moving the selected entries."
  in
  let failed_t =
    Arg.(value & flag & info ["f"; "failed"] ~doc)
  in
  let doc = "mark entries as done" in
  let man = [
    `S Manpage.s_description;
    `P "Parses the positional command line arguments as filter and moves the
    selected entries from the todo-file to the done-file file specified in the
    configuration. A confirmation prompt is displayed if more than one entry is
    selected (see -n, --noprompt)."
  ] in
  Term.(const td_do $ config_t $ verbose_t $ noprompt_t $ failed_t $ filter_t),
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
    selected entries from the done-file to the todo-file specified in the
    configuration. A confirmation prompt is displayed if more than one entry is
    selected (see -n, --noprompt)."
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
    let doc   = "Remove entries from the todo-file (default)." in
    let todo' = `Todo, Arg.info ["t"; "todo"] ~doc in
    let doc   = "Remove entries from the done-file file." in
    let done' = `Done, Arg.info ["d"; "done"] ~doc in
    let doc   = "Remove entries from both the todo-file and done-file files." in
    let all'  = `All, Arg.info ["a"; "all"] ~doc in
    Arg.(value & vflag `Todo [todo'; done'; all'])
  in
  let doc = "remove entries" in
  let man = [
    `S Manpage.s_description;
    `P "Parses the positional command line arguments as filter and removes the
    selected entries from the todo-file or done-file files specified in the
    configuration. A confirmation prompt is displayed if more than one entry is
    selected (see -n, --noprompt)."
  ] in
  Term.(const td_rm $ config_t $ verbose_t $ noprompt_t $ source_t $ filter_t),
  Term.info "rm" ~doc ~sdocs:Manpage.s_common_options ~man


let ls_cmd =
  let doc = "list entries" in
  let man = [
    `S Manpage.s_description;
    `P "List selected entries from the todo-file or done-file files with custom
    formatting and style."
  ] in
  let filter_t =
    Arg.(value & pos_all string [] & info [] ~docv:"FILTER")
    |> Term.(app (const (String.concat ~sep:" ")))
  in
  let source_t =
    let doc   = "List entries from the todo-file file (default)." in
    let todo' = `Todo, Arg.info ["t"; "todo"] ~doc in
    let doc   = "List entries from the done-file file." in
    let done' = `Done, Arg.info ["d"; "done"] ~doc in
    let doc   = "List entries from both the todo-file and done-file files." in
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

let sync_cmd =
  let doc = "synchronize todo and done files" in
  let man = [
    `S Manpage.s_description;
    `P "Executes the option `sync_cmd` from the config file as shell command."
  ] in
  Term.(const td_sync $ config_t),
  Term.info "sync" ~doc ~sdocs:Manpage.s_common_options ~man

let touch_cmd =
  let doc = "initialize todo and done files" in
  let man = [
    `S Manpage.s_description;
    `P "Touches the todo and done files specified in the configuration.
    This is a convenience command that makes sure that both the todo-file
    and done-file exist."
  ] in
  Term.(const td_touch $ config_t),
  Term.info "touch" ~doc ~sdocs:Manpage.s_common_options ~man

let default_cmd =
  let doc = "simple command line tool to manage todo lists" in
  let sdocs = Manpage.s_common_options in
  let exits = Term.default_exits in
  let default_ls config = td_ls config `Todo `None `Flat "<true>" in
  Term.(const default_ls $ config_t),
  Term.info "td" ~version:_version ~doc ~sdocs ~exits

let commands = [add_cmd; mod_cmd; ls_cmd; do_cmd; undo_cmd; rm_cmd; sync_cmd]
let () = Term.(exit @@ eval_choice default_cmd commands)

