open Tood

let _version = "0.1"

exception NotImplemented of string  


(*******************
 * File operations *
 *******************)

let default_config_path () =
  match Sys.getenv "TD_CONFIG" with
  | Some path -> path
  | None      -> "~/.config/td/conf"

let resolve_path path =
  match String.is_substring_at path ~pos:0 ~substring:"~" with
  | false -> path
  | true  -> match Sys.getenv "HOME" with
    | None -> path
    | Some home_dir ->
      String.substr_replace_first path ~pattern:"~" ~with_:home_dir

let parse_config_file path = Config.parse (resolve_path path)

let parse_todo_file ?(fail=false) path =
  try
    let lines = In_channel.read_lines (resolve_path path) in
    List.map ~f:Entry.of_string_exn lines
  with
  | Sys_error err as error -> begin match fail with
    | true  -> raise error
    | false -> Log.info
      ("file '" ^ path ^ "' cannot be read");
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



(*******************
 * Pretty printing *
 *******************)

let is_done ~done_index (index, entry) =
  match done_index, Int.(done_index <= index) with
  | 0, _     -> false
  | _, value -> value

let layout_entry ?(style=None) ?(offset=0) ~fmt ~fmt_date ~max_index entry =
  (String.make offset ' ') ^ (Entry.format_index ~fmt_date ~max_index fmt entry)

let layout_entries config layout indexed_entries =
  match Entry.max_index indexed_entries with
  | None -> Log.info_str "no entries to print"
  | Some max_index -> 
    match layout with
    | `Flat ->
      let fmt = Config.entry_fmt config in
      let fmt_date = Config.date_fmt config in
      let f = layout_entry ~fmt ~fmt_date ~max_index in
      List.map ~f indexed_entries |> String.concat ~sep:"\n"
    | `Tree ->
      let fmt = Config.entry_fmt_tree config in
      let fmt_date = Config.date_fmt config in
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



(***************
 * td commands *
 ***************)

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

let list_entries_if_verbose config verbose selected =
  match verbose with
  | false -> ()
  | true  ->
    print_endline "Selected entries:"; 
    selected |> layout_entries config `Flat |> print_endline

let wrap_config config cmd =
  let handle_config = function
  | Ok config -> cmd config
  | Error (`Invalid err) -> Log.err err
  | Error (`Not_found path) -> 
    Log.info ("config file " ^ path ^ " cannot be accessed, using fallback options"); 
    cmd Config.fallback
  in
  handle_config config

let wrap_error cmd = 
  try cmd () with
  | Sys_error err 
  | ArgumentError err 
  | NotImplemented err -> Log.err err
  | error -> raise error

let wrap config cmd = wrap_error (fun () -> wrap_config config cmd)

let move ?(f=fun x -> x) config verbose noprompt sel msg from_path to_path =
  let entries = Entry.index (parse_todo_file from_path) in
  let selected, unselected =
    Filter.split_indexed sel entries
  in
  match selected with
  | [] -> Log.info "no entries selected"
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
    | false -> Log.info "aborted"
    | true  ->
      write_todo_file from_path (Entry.deindex unselected);
      match to_path with
      | None -> ()
      | Some to_path ->
        let selected = List.map ~f:(fun (i,x) -> (i, f x)) selected in
        append_todo_file (Entry.deindex selected) to_path


(* td add *)

let _td_add config source entry_str =
  let path = match source with
    | `Todo -> Config.todo_path config
    | `Done -> Config.done_path config
  in
  let fmt_date = Config.date_fmt config in
  let entry = Entry.of_string_exn ~fmt_date entry_str in
  append_todo_file [entry] path

let td_add config source entry_str =
  wrap config (fun c -> _td_add c source entry_str)


(* td do *)

let _td_do config verbose noprompt failed filter =
  let fmt_date = Config.date_fmt config in
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
    (Config.todo_path config) (Some (Config.done_path config))

let td_do config verbose noprompt failed filter =
  wrap config (fun c -> _td_do c verbose noprompt failed filter)


(* td undo *)

let _td_undo config verbose noprompt filter =
  let fmt_date = Config.date_fmt config in
  let sel = Filter.of_string_exn ~fmt_date filter in
  let msg l =
    "You are about to mark " ^ l ^ " done entries as undone."
  in
  move
    config verbose noprompt sel msg
    (Config.done_path config) (Some (Config.todo_path config))

let td_undo config verbose noprompt filter =
  wrap config (fun c -> _td_undo c verbose noprompt filter)


(* td rm *)

let _td_rm config verbose noprompt source filter =
  let fmt_date = Config.date_fmt config in
  let sel = Filter.of_string_exn ~fmt_date filter in
  let msg, path = match source with
  | `Todo -> 
    let msg l =
      "You are about to delete " ^ l ^ " entries."
    in
    msg, Config.todo_path config
  | `Done ->
    let msg l =
      "You are about to delete " ^ l ^ " done entries."
    in
    msg, Config.done_path config
  | `All ->
    raise (NotImplemented "Flag --all not implemented yet. Wait for version 0.2")
  in
  move config verbose noprompt sel msg path None

let td_rm config verbose noprompt source filter =
  wrap config (fun c -> _td_rm c verbose noprompt source filter)


(* td mod *)

let modify config verbose noprompt sel msg path mods =
  let entries = parse_todo_file path |> Entry.index in
  match Filter.filter_indexed sel entries with
  | []       -> Log.info "no entries modified"
  | selected ->
    let () = list_entries_if_verbose config verbose selected in
    let proceed =
      match List.length selected, List.exists ~f:Mod.is_text mods with
      | 1, _ -> true
      | l, false -> if noprompt then true else prompt_confirmation (msg l) true
      | l, true  ->
          Log.warn ("replacing text of " ^ Int.to_string l ^ " entries at once");
          if noprompt then true else prompt_confirmation (msg l) true
    in
    match proceed with
    | false -> Log.info "aborted"
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
  let sel, mods = parse_mod_args (Config.date_fmt config) str in
  let path, msg_frac = match source with
    | `Todo -> Config.todo_path config, " entries."
    | `Done -> Config.done_path config, " done entries."
    | `All -> raise 
      (NotImplemented "flag --all not implemented yet - wait for version 0.2")
  in
  let msg l = ("You are about to modify " ^ Int.to_string l ^ msg_frac) in
  modify config verbose noprompt sel msg path mods

let td_mod config noprompt verbose source filter_mod_str = 
  wrap config (fun c -> _td_mod c noprompt verbose source filter_mod_str)


(* td ls *)

let _td_ls config source order layout filter =
  let fmt_date = Config.date_fmt config in
  let sel = Filter.of_string_exn ~fmt_date filter in
  let entries = match source with
    | `Todo -> parse_todo_file (Config.todo_path config)
    | `Done -> parse_todo_file (Config.done_path config)
    | `All  -> 
      parse_todo_file (Config.todo_path config)
      @ parse_todo_file (Config.done_path config)
    in
    Entry.index entries
    |> Filter.filter_indexed sel
    |> sort_entries order
    |> layout_entries config layout
    |> print_endline

let td_ls config source order layout filter =
  wrap config (fun c -> _td_ls c source order layout filter)


(* td sync *)

let _td_sync config = 
  match Config.sync_cmd config with
  | None     -> Log.info "no sync command configured"
  | Some cmd -> match Unix.system cmd with
    | Ok ()   -> Log.info "synchronization successfull"
    | Error _ -> raise (Failure "sync command exited unsuccessfully")

let td_sync config  = wrap config (fun c -> _td_sync c)


(* td init *)

let _td_init quiet config = 
  let input_path default = 
    match In_channel.(input_line stdin) with
    | None | Some "" -> default
    | Some path -> path
  in
  let touch_tood_files config =
    let todo_path = Config.todo_path config |> resolve_path in
    let done_path = Config.done_path config |> resolve_path in
    let f _ = () in
    Out_channel.with_file todo_path ~append:true ~f;
    Out_channel.with_file done_path ~append:true ~f
  in
  match config with
  | Ok config ->
    if not quiet then Log.info "a valid td config file already exists";
    touch_tood_files config
  | Error (`Invalid err) ->
    if not quiet then Log.info "a td config file already exists but cannot be parsed";
    Log.err err
  | Error (`Not_found path) ->
    let open Config in
    let dir = Filename.dirname path in
    let path = Filename.(concat dir (basename path)) in
    let todo_path = Filename.concat dir "todo.tood" in
    let done_path = Filename.concat dir "done.tood" in
    print_endline 
      ("A default td configuration file will be created at '" ^ path ^ "'.");
    print_string ("Path for the todo file (default: '" ^ todo_path ^ "'): ");
    Out_channel.(flush stdout);
    let todo_path = input_path todo_path in
    print_string ("Path for the done file (default: '" ^ done_path ^ "'): ");
    Out_channel.(flush stdout);
    let done_path = input_path done_path in
    let config = { fallback with todo_path; done_path } in
    Out_channel.write_all path ~data:(Config.to_string config);
    touch_tood_files config;
    List.iter ~f:print_endline [
      "The configuration file was created successfully."
    ; "Start adding entries with 'td add ...'."
    ; "For general usage information about td look up 'td init --help'."
    ]

let td_init quiet config = wrap_error (fun () -> _td_init quiet config)



(************************
 * command line parsing *
 ************************)

open Cmdliner

let config_t =
  let doc =
    "Path to the configuration file. If provided, this argument overrides the
    default path ~/.config/td/conf and the environment variable TD_CONFIG."
  in
  let path =
    Arg.(value & opt string (default_config_path ()) & info ["c"; "config"]
    ~doc ~docv:"CONFIG-FILE")
  in
  Term.(const parse_config_file $ path)

let add_cmd =
  let doc = "create new entries" in
  let man = Man.add in
  let source_t =
    let doc   = "Add entry to todo-file (default)." in
    let todo' = `Todo, Arg.info ["t"; "todo"] ~doc in
    let doc   = "Add entry to done-file." in
    let done' = `Done, Arg.info ["d"; "done"] ~doc in
    Arg.(value & vflag `Todo [todo'; done'])
  in
  let entry_t =
    Arg.(non_empty & pos_all string [] & info [] ~docv:"TODO")
    |> Term.(app (const (String.concat ~sep:" ")))
  in
  Term.(const td_add $ config_t $ source_t $ entry_t),
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
  let man = Man.mod_ in
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
  let man = Man.do_ in
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
  let man = Man.undo in
  Term.(const td_undo $ config_t $ verbose_t $ noprompt_t $ filter_t),
  Term.info "undo" ~doc ~sdocs:Manpage.s_common_options ~man

let rm_cmd =
  let doc = "Filter applied to select entries." in
  let filter_t =
    Arg.(non_empty & pos_all string [] & info [] ~doc ~docv:"FILTER")
    |> Term.(app (const (String.concat ~sep:" ")))
  in
  let doc = "List the selected entries." in
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
  let man = Man.rm in
  Term.(const td_rm $ config_t $ verbose_t $ noprompt_t $ source_t $ filter_t),
  Term.info "rm" ~doc ~sdocs:Manpage.s_common_options ~man


let ls_cmd =
  let doc = "list entries" in
  let man = Man.ls in
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
  let doc = "execute a configured synchronization command" in
  let man = Man.sync in
  Term.(const td_sync $ config_t),
  Term.info "sync" ~doc ~sdocs:Manpage.s_common_options ~man

let init_cmd =
  let doc =
    "Suppress informative output of the command."
  in
  let quiet_t =
    Arg.(value & flag & info ["q"; "quiet"] ~doc)
  in
  let doc = "initialize configuration and tood files" in
  let man = Man.init in
  Term.(const td_init $ quiet_t $ config_t),
  Term.info "init" ~doc ~sdocs:Manpage.s_common_options ~man

let default_cmd =
  let doc = "simple but functional todo list management on the terminal" in
  let sdocs = Manpage.s_common_options in
  let man = Man.main in
  let exits = Term.default_exits in
  let default_ls config = td_ls config `Todo `None `Flat "<true>" in
  Term.(const default_ls $ config_t),
  Term.info "td" ~version:_version ~doc ~sdocs ~exits ~man



(********************
 * td main function *
 ********************)

let () = 
  let commands = [
      add_cmd
    ; mod_cmd
    ; ls_cmd
    ; do_cmd
    ; undo_cmd
    ; rm_cmd
    ; sync_cmd
    ; init_cmd
  ] in
  Term.(exit @@ eval_choice default_cmd commands)

