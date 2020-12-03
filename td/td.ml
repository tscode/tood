open Tood
open Printf

let _version = "0.1dev"

exception NotImplemented of string

module Util = struct

let wrap_config config listname cmd =
  let run config =
    let name = match listname = "" with
    | true  -> List.nth (Config.names config) 0
    | false -> listname
    in
    match Config.options config name with
    | None   -> sprintf "list '%s' does not exist" listname |> Log.err
    | Some options -> cmd options listname
  in
  match config with
  | `Ok config      -> run config
  | `Invalid err    -> Log.err err
  | `Not_found path -> 
    sprintf "config file %s cannot be accessed" path |> Log.err

let wrap_error cmd = 
  try cmd () with
  | Sys_error err 
  | ArgumentError err 
  | NotImplemented err -> Log.err err
  | error -> raise error

let wrap config listname cmd =
  wrap_error (fun () -> wrap_config config listname cmd)

let resolve_path path =
  if String.length path = 0 then path
  else match String.get path 0 == '~' with
  | false -> path
  | true  ->
  match Sys.getenv_opt "HOME" with
  | None -> Log.warn "cannot access environment variable HOME"; path
  | Some home_dir -> home_dir ^ String.(sub path 1 (length path - 1))

let parse_list path =
  try Io.read_lines (resolve_path path) |> List.map Entry.of_string_exn with
  | ArgumentError err ->
    let msg = sprintf "faulty todo list '%s': %s" path err in
    raise (ArgumentError msg)
  | Sys_error _ ->
    let msg = sprintf "file '%s' cannot be accessed" path in
    raise (ArgumentError msg)

let write_list ?(append=false) entries path =
  List.map Entry.to_string_strict entries
  |> Io.write_lines ~append (resolve_path path)

let prompt_confirmation msg =
  let msg = msg ^ " Proceed? [Y/n] " in
  print_string msg; flush_all ();
  let rec read () =
    match input_line stdin |> String.lowercase_ascii with
    | "n" | "no"  -> false
    | "" | "y" | "ye" | "yes" -> true
    | _ -> print_endline "Please type 'y' or 'n' for yes or no"; read ()
  in read ()

let move ?(f=fun x -> x) ?summary ?prompt filter from_path to_path =
  let entries = Entry.index (parse_list from_path) in
  let sel, unsel = Filter.split_indexed filter entries in
  match sel with
  | [] -> Log.info "no entries selected"
  | _  ->
  Option.iter (fun s -> s sel |> printf "Selected entries: %s\n") summary;
  let proceed =
    let n = List.length sel in match prompt with
    | Some prompt -> if n > 1 then prompt n |> prompt_confirmation else true
    | None -> true
  in
  match proceed with
  | false -> Log.info "action aborted"
  | true  ->
  write_list (Entry.deindex unsel) from_path;
  match to_path with
  | None -> ()
  | Some to_path ->
    let sel = List.map (fun (i, x) -> (i, f x)) sel in
    write_list ~append:true (Entry.deindex sel) to_path

let parse_order str =
  let open Angstrom in
  let open Parser in
  let word a b = string a *> return b in
  let part = choice
    [ word "index" Entry.Index
    ; word "prio" Entry.Prio
    ; word "text" Entry.Text
    ; word "date" Entry.Date
    ; word "context" Entry.Context
    ; word "project" Entry.Project ]
  in
  let may_invert opt part = match opt with
  | None -> part, false
  | Some _ -> part, true
  in
  let word = lift2 may_invert (opt (char '~')) part in
  let parser = sep_by (char ',') word in
  match parse_string ~consume:Consume.All parser str with
  | Ok parts -> parts
  | Error _ -> raise (ArgumentError (sprintf "invalid order argument '%s'" str))

end

module Common_terms = struct

open Cmdliner

let quiet_flag =
  let doc = "Suppress information output." in
  Arg.(value & flag & info ["q"; "quiet"] ~doc)

let verbose_flag =
  let doc = "Show additional information." in
  Arg.(value & flag & info ["v"; "verbose"] ~doc)

let noprompt_flag =
  let doc = "Suppress confirmation prompt if several entries are selected." in
  Arg.(value & flag & info ["n"; "noprompt"] ~doc)

let filter ~optional =
  let op = if optional then Arg.value else Arg.non_empty in
  Arg.(op & pos_all string [] & info [] ~docv:"FILTER")
  |> Term.(app (const (String.concat " ")))

let source_all =
  let doc   = "Operate on the done-file." in
  let done' = `Done, Arg.info ["d"; "done"] ~doc in
  let doc   = "Operate on both the todo- and done-file." in
  let all'  = `All, Arg.info ["a"; "all"] ~doc in
  Arg.(value & vflag `Todo [done'; all'])

let source =
  let doc   = "Operate on the done-file." in
  let done' = `Done, Arg.info ["d"; "done"] ~doc in
  Arg.(value & vflag `Todo [done'])

let order ~optional =
  let order = function
    | "none" -> Ok None
    | arg    ->
    match Util.parse_order arg with
    | ord -> Ok (Some ord)
    | exception ArgumentError err -> Error (`Msg err)
  in
  let doc = "Ordering of the entries. Must be a comma separated list of
  keywords. Allowed keywords are $(b,index), $(b,prio), $(b,text), $(b,context),
  $(b,project), $(b,date). The character $(b,~) may be prepended to a keyword to
  invert the order. For example, the argument 'prio,~date,project' first sorts
  according to priority (higher wins), then according to the first date tag
  (later dates win due to the inversion), and finally according to the first
  project tag (in alphabetic order)."
  in
  let docv = "ORDER" in
  let value = match optional with
  | true -> Arg.(value & opt string "none" & info ["o"; "order"] ~docv ~doc)
  | false -> Arg.(value & pos 0 string "none" & info [] ~docv ~doc)
  in
  Term.(app (const order)) value |> Term.term_result

end

module Td_init = struct

let input_string default =
  flush_all ();
  match input_line stdin with
  | "" -> default
  | path -> path

let touch_file path =
  match Io.write_lines ~append:true path [] with
  | () -> Log.info (sprintf "touched file %s" path)
  | exception _ ->
    let msg = sprintf "cannot touch file %s %s" path
     "(check if folder exist and permissions are set)" in
    raise (ArgumentError msg)

let cmd' config listname quiet = 
  let touch options =
    touch_file (Config.get options "todo-path" |> Util.resolve_path);
    touch_file (Config.get options "done-path" |> Util.resolve_path)
  in
  match config with
  | `Ok config ->
    if not quiet then
    Log.info "a valid config file already exists";
    List.iter
      (fun name -> Config.options_exn config name |> touch)
      (Config.names config)
  | `Invalid err ->
    if not quiet then
    Log.info "a config file already exists but cannot be parsed";
    Log.err err
  | `Not_found path ->
    let dir = Filename.dirname path in
    let path = Filename.(concat dir (basename path)) in
    printf "A default config file will be created at '%s'.\n" path;
    let listname =
      let default = if listname = "" then "default" else listname in
      printf "Name of the default list ('%s'): " default;
      input_string default
    in
    let todo_path =
      let default = Filename.concat dir "todo.txt" in
      printf "Path of the todo-file (default: '%s'): " default;
      input_string default
    in
    let done_path =
      let default = Filename.concat dir "done.txt" in
      printf "Path of the done-file (default: '%s'): " default;
      input_string default
    in
    let config = Config.default ~listname todo_path done_path in
    let options = Config.options_exn config listname in
    Io.write_all path (Config.to_string config);
    touch options;
    List.iter print_endline
    [ "The configuration file was created successfully."
    ; "You can start adding entries with 'td add ...'."
    ; "For general usage information, look up 'td init --help'." ]

let cmd config listname quiet =
  Util.wrap_error (fun () -> cmd' config listname quiet)

open Cmdliner

let man =
  let entry_placeholders =
    [ `P "$(b,%i) : index of the entry"
    ; `P "$(b,%I) : index of the entry with right padding"
    ; `P "$(b,%J) : index of the entry with left padding"
    ; `P "$(b,%r) : entry priority (?, -, !)"
    ; `P "$(b,%s) : entry text"
    ; `P "$(b,%t) : entry tags without tagmarks"
    ; `P "$(b,%T) : entry tags with tagmarks"
    ; `P "$(b,%p) : project tags without tagmarks"
    ; `P "$(b,%P) : project tags with tagmarks"
    ; `P "$(b,%c) : context tags without tagmarks"
    ; `P "$(b,%C) : context tags with tagmarks"
    ; `P "$(b,%d) : date tags without tagmarks"
    ; `P "$(b,%D) : date tags with tagmarks" ]
  in
  let date_placeholders =
    [ `P "$(b,%y) : year"
    ; `P "$(b,%M) : month in the current year"
    ; `P "$(b,%m) : month in the current year (always two digits)"
    ; `P "$(b,%D) : day in the current month"
    ; `P "$(b,%d) : day in the current month (always two digits)" ]
  in
  let config_options =
    [ `P "- $(b,todo-path) : file where active todo entries are stored"
    ; `P "- $(b,done-path) : file where done todo entries are stored"
    ; `P "- $(b,entry-fmt) : entry format when printing / listing"
    ; `P "- $(b,entry-fmt-tree): entry format when printing in tree layout"
    ; `P "- $(b,date-fmt) : date format used for printing $(b,and) parsing"
    ; `P "- $(b,sync-cmd) : shell command called when executing $(b,td sync)" ]
  in
  let style_options =
    [ `P "- $(b,index-style)   : style of the index number"
    ; `P "- $(b,prio-style)    : style of the priority character"
    ; `P "- $(b,text-style)    : style of the entry text"
    ; `P "- $(b,tag-style)     : style of any tags"
    ; `P "- $(b,project-style) : style of project tags"
    ; `P "- $(b,context-style) : style of context tags"
    ; `P "- $(b,date-style)    : style of date tags" ]
  in
  let description = [
  `S Manpage.s_description;
  `P "Generates a td configuration file with default options (if it does not
  already exist) and touches the specified todo- and done-files, checking if
  they can be accessed. The default path of the configuration file is
  $(b,~/config/td/config). This can be overwritten by setting the environment
  variable $(b,TD_CONFIG) or by providing the command line argument
  $(b,--config)."
  ]
  in
  let configuration = [
  `S "CONFIGURATION";
  `P "The td configuration file defaults to $(b,~/.config/td/config) unless
  the environment variable $(b,TOOD_CONFIG) is defined. It can also be specified
  on invocation by the $(b,--config) option for all subcommands.";
  `P "Each line in the configuration file must either be a $(b,LISTNAME) or a
  pair $(b,KEY : VALUE). Listnames and keys may only contain the characters
  'a-z|A-Z|0-9|-|_'. Each listname opens a new section in the configuration
  file, with options that apply to the corresponding list exclusively.
  General options that apply to all lists (unless overwritten) can be put in the
  top of the configuration file.";
  `P "The following keys are accepted:";
  ] @ config_options @ [
  `P "The placeholders to be used in format strings are documented in the
  section $(b,FORMATTING) below.";
  `P "It is also possible to specify the style (e.g., color or boldness) of
  different parts of an entry when printing them to an ANSI-compliant terminal.
  A valid style consists of a space-separated list of one of the properties
  $(b,bold), $(b,dim), $(b,underlined), $(b,blink), $(b,COLOR), or
  $(b,on-COLOR). The value $(b,COLOR) can either be a string (like 'white',
  'yellow', or 'light-cyan') for one of the standart 16 ANSI colors, or it can
  be a number corresponding to the code of a 256 ANSI color.
  If the style should only apply for entries of high / low priority, the
  characters $(b,!) and $(b,?) can be used. An examplary style value would be";
  `P "  blue ! bold red on-green ? underlined grey";
  `P "which would mean: blue foreground color if the priority is normal; bold
  font with red foreground and green background if the priority is high;
  underlined font with grey foreground if the priority is low.";
  `P "The following styles can be set:"
  ] @ style_options
  in
  let entries = [
  `S "ENTRIES";
  `P "Entries consist of three parts: a priority, the actual entry text, and a
  list of tags. Its textual representation is";
  `P "PRIORITY TEXT TAG1 TAG2...";
  `P "The priority can be low, middle, or high and is represented by the
  characters $(b,?), $(b,-), and $(b,!). Tags always start with the character
  $(b,+) and come in three flavors: context tags (like '+work'), project
  tags that contain at least one $(b,/) (like '+project/' or
  '+project/subproject'), and date tags (like '+2020-05-12'). Note that td
  uses the date format $(b,%y-%m-%d) for storage, while custom date formats
  for user interaction can be specified in the config file. Examples for valid
  tood entries are:";
  `P "- this entry is moderately important +example +tood/documentation";
  `Noblank; `P "! this entry is important +and/has/many/subprojects +2005-08-10";
  `P "Entries can be equipped with an arbitrary number fo tags. Due to the
  significance of the character $(b,+), the text of entries can currently not
  contain this symbol." ]
  in
  let filters = [
  `S "FILTERS";
  `P "Td can filter and select tood entries with a flexible boolean filtering
  language. Basic filters are '+TAG', '\"text\"' or the symbols '?', '-', and
  '!' for exact matching of tags, text, and priorities. Indices like '5' can
  also be used to filter the entries based on their position in a tood file.
  Further supported filtering patterns are:";
  `P "- 'a..b' matches entries with index / date between a and b (inclusive)";
  `Noblank; `P "- 'and' and 'or' combine two filters logically";
  `Noblank; `P "- 'not' negates a filter";
  `Noblank; `P "- '(...)' groups filters";
  `Noblank; `P "- '/key/' searches for 'key' as substring in (sub)projects";
  `Noblank; `P "- plain words (except 'not', 'and', 'or') are matched in text and tags";
  `P "Some examples of valid filters are:";
  `P "- '+work and (not +project/)' # the parenthesis here is optional";
  `Noblank; `P "- '1..7 or 9..15' # index must be between 1 and 7 or 9 and 15";
  `Noblank; `P "- 'meet and ..2021-5-18' # entries containing 'meet' before 2021-5-18"
  ]
  in
  let formatting = [
  `S "FORMATTING";
  `P "The following placeholders can be used when specifying format entries in
  the config options $(b,entry-fmt) and $(b,entry-fmt-tree). Adding an
  underscore '_' after a pattern adds a conditional space, which will only be
  inserted if the pattern is not replaced by the empty string '', which can
  happen in case of empty lists of tags."
  ] @ entry_placeholders @ [
  `P "The following placeholders are used when specifying a date format for the
  config option $(b,date-fmt). Date formats have to parsable and are thus only
  valid if they contain each of the listed placeholders exactly once."
  ] @ date_placeholders
  in List.concat
  [ description
  ; configuration
  ; entries
  ; filters
  ; formatting ]

let term config_t listname_t =
  let quiet_t = Common_terms.quiet_flag in
  let doc = "initialize a configuration file" in
  Term.(const cmd $ config_t $ listname_t $ quiet_t),
  Term.info "init" ~doc ~sdocs:Manpage.s_common_options ~man

end

module Td_add = struct

let cmd' options _listname target entry_str =
  let path = match target with
  | `Todo -> Config.get options "todo-path"
  | `Done -> Config.get options "done-path"
  in
  let fmt = Config.get options "date-fmt" |> Date.fmt_exn in
  let entry = Entry.of_string_exn ~fmt entry_str in
  Util.write_list ~append:true [entry] path

let cmd config listname target entry_str =
  Util.wrap config listname (fun o l -> cmd' o l target entry_str)

open Cmdliner

let man = [
  `S Manpage.s_description;
  `P "Parses the provided command line arguments as todo-list entry and adds it
  to the file specified in the configuration. The entry has the format";
  `P "[-!?] text +tag1 +tag2 ...";
  `P "where the priority at the beginning is optional (it defaults to medium
  priority '-'). For documentation on the syntax of tags, see $(b,td init
  --help) and the examples below.";
  `S Manpage.s_examples;
  `P "The following are examples that add entries to the todo file:";
  `P "td add first entry";
  `Noblank; `P "td add ! an important second entry +example +project/subproject";
  `Noblank; `P "td add an entry with date tag +2055-07-28";
  `P "Note that the format for the input of date tags depends on the config option
  $(b,date-fmt). So if 'date-fmt : %d.%m.%y' was set in the config file, the
  date tag above would have to be '+28.07.2055'. The format options for entries,
  on the other hand, are only used for listing / printing."
]

let entry_t =
  Arg.(non_empty & pos_all string [] & info [] ~docv:"ENTRY")
  |> Term.(app (const (String.concat " ")))

let term config_t listname_t =
  let source_t = Common_terms.source in
  let doc = "create new entries" in
  Term.(const cmd $ config_t $ listname_t $ source_t $ entry_t),
  Term.info "add" ~doc ~sdocs:Manpage.s_common_options ~man

end

module Td_ls = struct

let is_done ~done_index (index, _entry) =
  match done_index, done_index <= index with
  | 0, _     -> false
  | _, value -> value

let get_style options entry kind = let open Entry in
  let style = match kind with
  | Index   -> Config.get_printer options "index-style"
  | Prio    -> Config.get_printer options "prio-style"
  | Text    -> Config.get_printer options "text-style"
  | Tag     -> Config.get_printer options "tag-style"
  | Project -> Config.get_printer options "project-style"
  | Context -> Config.get_printer options "context-style"
  | Date    -> Config.get_printer options "date-style"
  in style ~prio:(prio entry)

let layout_entry ?(offset=0) max_index style fmt entry_fmt entry =
  let str = Entry.format_indexed ~max_index ~style ~fmt entry_fmt entry in
  (String.make offset ' ') ^ str

let layout_entries options layout indexed_entries =
  match Entry.max_index indexed_entries with
  | None -> Log.info_str "no entries to print"
  | Some max_index -> 
    let style = get_style options in
    match layout with
    | `Flat ->
      let entry_fmt = Config.get options "entry-fmt" in
      let fmt = Config.get options "date-fmt" |> Date.fmt_exn in
      let f = layout_entry max_index style fmt entry_fmt in
      List.map f indexed_entries |> String.concat "\n"
    | `Tree ->
      let entry_fmt = Config.get options "entry-fmt-tree" in
      let fmt = Config.get options "date-fmt" |> Date.fmt_exn in
      let paths (_, entry) = match Entry.project_tags entry with
        | []       -> [[]]
        | projects -> projects
      in
      let max a b = if a > b then a else b in
      let tree = Tree.of_leaves ~paths indexed_entries in
      let fname path name = 
        let offset = max 0 (List.length path - 1) in 
        String.make offset ' ' ^ Symbol.to_string name ^ "/"
      in
      let f path entry =
        let offset = max 0 (List.length path - 1) in
        layout_entry ~offset max_index style fmt entry_fmt entry
      in
      Tree.collect ~fname f tree |> List.tl |> String.concat "\n"

let sort_entries order entries =
  match order with
  | None -> entries
  | Some order -> Entry.sort_indexed order entries

let cmd' options _listname source order layout filter =
  let fmt = Config.get options "date-fmt" |> Date.fmt_exn in
  let sel = Filter.of_string_exn ~fmt filter in
  let entries = match source with
  | `Todo -> Util.parse_list (Config.get options "todo-path")
  | `Done -> Util.parse_list (Config.get options "done-path")
  | `All  -> 
    Util.parse_list (Config.get options "todo-path")
    @ Util.parse_list (Config.get options "done-path")
  in
  Entry.index entries
  |> Filter.filter_indexed sel
  |> sort_entries order
  |> layout_entries options layout
  |> print_endline

let cmd config listname source order layout filter =
  Util.wrap config listname (fun o l -> cmd' o l source order layout filter)

open Cmdliner

let man = [
  `S Manpage.s_description;
  `P "List selected entries from the todo- or done-file. The configuration
  options entry-fmt and entry-fmt-tree (if the flag --tree is provided)
  determine how the entries are formatted."
]

let layout_t =
  let doc   = "List entries as project tree." in
  let tree' = `Tree, Arg.info ["t"; "tree"] ~doc in
  Arg.(value & vflag `Flat [tree'])

let term config_t listname_t =
  let doc = "list entries" in
  let filter_t = Common_terms.filter ~optional:true in
  let source_t = Common_terms.source_all in
  let order_t  = Common_terms.order ~optional:true in
  Term.(const cmd $ config_t $ listname_t $ source_t
                    $ order_t $ layout_t $ filter_t),
  Term.info "ls" ~doc ~sdocs:Manpage.s_common_options ~man

end

module Td_sort = struct

let cmd' options _listname source order =
  let paths = match source with
  | `Todo -> [Config.get options "todo-path"]
  | `Done -> [Config.get options "done-path"]
  | `All -> List.map (Config.get options) ["todo-path"; "done-path"]
  in
  let reorder path =
    let entries = Util.parse_list path
    |> Entry.index
    |> Td_ls.sort_entries order
    in
    Util.write_list (Entry.deindex entries) path
  in
  List.iter reorder paths
  
let cmd config listname source order =
  Util.wrap config listname (fun o l -> cmd' o l source order)

open Cmdliner

let man = [
  `S Manpage.s_description;
  `P "Sort entries in todo-file and/or done-file according to a specified
  order."
]

let term config_t listname_t =
  let doc = "sort entries" in
  let source_t = Common_terms.source_all in
  let order_t  = Common_terms.order ~optional:false in
  Term.(const cmd $ config_t $ listname_t $ source_t $ order_t),
  Term.info "sort" ~doc ~sdocs:Manpage.s_common_options ~man

end

module Td_do = struct

let optional_summary options = function
  | true -> Td_ls.layout_entries options `Flat |> Option.some
  | false -> None

let optional_prompt msg = function
  | true -> msg |> Option.some
  | false -> None

let cmd' options _listname verbose noprompt failed filter =
  let fmt = Config.get options "date-fmt" |> Date.fmt_exn in
  let filter = Filter.of_string_exn ~fmt filter in
  let summary = optional_summary options verbose in
  let prompt = optional_prompt
    (sprintf "You are about to mark %d entries as done.") (not noprompt)
  in
  let f = match failed with
    | true  -> Mod.(apply (of_string_exn "+failed"))
    | false -> fun entry -> entry
  in
  Util.move ~f ?summary ?prompt filter
    (Config.get options "todo-path")
    (Some (Config.get options "done-path"))

let cmd config listname verbose noprompt failed filter =
  Util.wrap config listname (fun o l -> cmd' o l verbose noprompt failed filter)

open Cmdliner

let man = [
  `S Manpage.s_description;
  `P "Parses the positional command line arguments as filter and moves the
  selected entries from the configured todo- to the done-file. By default, a
  confirmation prompt is displayed if more than one entry is selected. The
  syntax for filters is documented under $(b,td init --help)."
]

let failed_t   =
  let doc = "Add the tag +failed when moving the selected entries." in
  Arg.(value & flag & info ["f"; "failed"] ~doc)

let term config_t listname_t =
  let doc = "mark entries as done" in
  let filter_t   = Common_terms.filter ~optional:false in
  let verbose_t  = Common_terms.verbose_flag in
  let noprompt_t = Common_terms.noprompt_flag in
  Term.(const cmd $ config_t $ listname_t $ verbose_t
                  $ noprompt_t $ failed_t $ filter_t),
  Term.info "do" ~doc ~sdocs:Manpage.s_common_options ~man

end

module Td_undo = struct

let cmd' options _listname verbose noprompt filter =
  let fmt = Config.get options "date-fmt" |> Date.fmt_exn in
  let filter = Filter.of_string_exn ~fmt filter in
  let summary = Td_do.optional_summary options verbose in
  let prompt = Td_do.optional_prompt
    (sprintf "You are about to mark %d done entries as undone.") (not noprompt)
  in
  Util.move ?summary ?prompt filter
    (Config.get options "done-path")
    (Some (Config.get options "todo-path"))

let cmd config listname verbose noprompt filter =
  Util.wrap config listname (fun o l -> cmd' o l verbose noprompt filter)

open Cmdliner

let man = [
  `S Manpage.s_description;
  `P "Parses the positional command line arguments as filter and moves the
  selected entries from the configured done- to the todo-file. By default, a
  confirmation prompt is displayed if more than one entry is selected. The
  syntax for filters is documented under $(b,td init --help)."
]

let term config_t listname_t =
  let doc = "mark entries as undone" in
  let verbose_t  = Common_terms.verbose_flag in
  let noprompt_t = Common_terms.noprompt_flag in
  let filter_t = Common_terms.filter ~optional:false in
  Term.(const cmd $ config_t $ listname_t $ verbose_t $ noprompt_t $ filter_t),
  Term.info "undo" ~doc ~sdocs:Manpage.s_common_options ~man

end

module Td_rm = struct

let todo_prompt =
  Td_do.optional_prompt
  (sprintf "You are about to delete %d entries.")

let done_prompt =
  Td_do.optional_prompt
  (sprintf "You are about to delete %d entries that are marked as done.")

let cmd' options _listname verbose noprompt source filter =
  let fmt = Config.get options "date-fmt" |> Date.fmt_exn in
  let filter = Filter.of_string_exn ~fmt filter in
  let summary = Td_do.optional_summary options verbose in
  let prompt, path = match source with
  | `Todo -> todo_prompt (not noprompt), Config.get options "todo-path"
  | `Done -> done_prompt (not noprompt), Config.get options "done-path"
  in
  Util.move ?summary ?prompt filter path None

let cmd config listname verbose noprompt source filter =
  Util.wrap config listname (fun o l -> cmd' o l verbose noprompt source filter)

open Cmdliner

let man = [
  `S Manpage.s_description;
  `P "Parses the positional command line arguments as filter and removes the
  selected entries from the configured todo- or done-file. By default, a
  confirmation prompt is displayed if more than one entry is selected. The
  syntax for filters is documented under $(b,td init --help)."
]
 
let term config_t listname_t =
  let doc = "remove entries" in
  let source_t   = Common_terms.source in
  let filter_t   = Common_terms.filter ~optional:false in
  let verbose_t  = Common_terms.verbose_flag in
  let noprompt_t = Common_terms.noprompt_flag in
  Term.(const cmd $ config_t $ listname_t $ verbose_t
                  $ noprompt_t $ source_t $ filter_t),
  Term.info "rm" ~doc ~sdocs:Manpage.s_common_options ~man

end

module Td_mod = struct

let maybe_prompt l = function
  | Some prompt -> prompt l |> Util.prompt_confirmation
  | None -> true

let modify ?summary ?prompt filter path mods =
  let entries = Util.parse_list path |> Entry.index in
  match Filter.filter_indexed filter entries with
  | []  -> Log.info "no entries were modified"
  | sel ->
  Option.iter (fun s -> s sel |> printf "Selected entries:\n%s\n") summary;
  let proceed =
    match List.length sel, List.exists Mod.is_text mods with
    | 1, _ -> true
    | l, false -> maybe_prompt l prompt
    | l, true  ->
      Log.warn (sprintf "replacing text of %d entries at once" l);
      maybe_prompt l prompt
  in
  match proceed with
  | false -> Log.info "aborted"
  | true  -> 
  entries
  |> Filter.map_indexed filter (Mod.apply_list mods)
  |> Entry.deindex
  |> (fun x -> Util.write_list x path)

let parse_mod_args fmt str =
  let open Angstrom in
  let open Parser in
  let tup x y = (x, y) in
  let p = lift2 tup (filter ~fmt () <* ws) (mods ~fmt ()) in
  match parse_string ~consume:All p str with
  | Ok t    -> t
  | Error _ ->
  let msg = sprintf "wrong mod syntax: '%s'" str in
  let msg = msg ^ " (should be 'filter mod1 [mod2 ...]')" in
  raise (ArgumentError msg)

let todo_prompt =
  Td_do.optional_prompt
  (sprintf "You are about to modify %d entries.")

let done_prompt =
  Td_do.optional_prompt
  (sprintf "You are about to modify %d entries marked as done.")

let cmd' options _listname noprompt verbose source str =
  let fmt = Config.get options "date-fmt" |> Date.fmt_exn in
  let filter, mods = parse_mod_args fmt str in
  let summary = Td_do.optional_summary options verbose in
  let prompt, path = match source with
  | `Todo -> todo_prompt (not noprompt), Config.get options "todo-path"
  | `Done -> done_prompt (not noprompt), Config.get options "done-path"
  in
  modify ?summary ?prompt filter path mods

let cmd config listname noprompt verbose source filter_mod_str = 
  Util.wrap config listname
  (fun o l -> cmd' o l noprompt verbose source filter_mod_str)

open Cmdliner

let man = [
  `S Manpage.s_description;
  `P "Modify an entry or a selection of entries. By default, if more than one
  entry is modified, a confirmation prompt is displayed. The syntax for the
  arguments of $(b,td mod) is";
  `P "filter mod1 [mod2 mod3...]";
  `P "where each modification can have the form (with '|' denoting choices)";
  `P "? | - | ! | \"TEXT\" | +TAG | ~TAG";
  `P "Note that replacement text has to be quoted with '\"' and removing tags
  requires prepending '~' instead of '+' to the tag. The syntax for filters and
  tags is documented at $(b,td init --help).";
  `S Manpage.s_examples;
  `P "The following command renames the tag '+toberemoved' to '+tobeadded'
  for all entries that have the former tag and that have an index smaller or
  equal to 55.";
  `P "td mod +toberemoved and ..55 ~toberemoved +tobeadded";
  `P "This command changes the priority of entry 7 to high and adds the
  tag '+tobeadded'.";
  `P "td mod 7 ! +tobeadded";
]

let filter_mod_t =
  let doc =
    "Filter to select the entries to be modified and (space separated) list of
    modifications. See $(b,td init --help) for documentation of the filter
    syntax"
  in
  Arg.(non_empty & pos_all string [] & info [] ~doc ~docv:"FILTER MODS")
  |> Term.(app (const (String.concat " ")))

let term config_t listname_t =
  let source_t = Common_terms.source in
  let verbose_t = Common_terms.verbose_flag in
  let noprompt_t = Common_terms.noprompt_flag in
  let doc = "modify entries" in
  Term.(const cmd $ config_t $ listname_t $ noprompt_t
                  $ verbose_t $ source_t $ filter_mod_t),
  Term.info "mod" ~doc ~sdocs:Manpage.s_common_options ~man

end

module Td_sync = struct

let cmd' options _listname = 
  match Config.get_safe options "sync-cmd" with
  | None     -> Log.info "no synchronization command set in config file"
  | Some cmd ->
  sprintf "executing command '%s'...\n" cmd |> Log.info;
  match Unix.system cmd with
  | Unix.WEXITED 0 -> Log.info "synchronization successful"
  | _ -> raise (Failure "synchronization was not successful")

let cmd config listname =
  Util.wrap config listname (fun o l -> cmd' o l)

open Cmdliner

let man = [
  `S Manpage.s_description;
  `P "Executes the value of the configuration option $(b,sync-cmd) as shell
  command."
]

let term config_t listname_t =
  let doc = "execute a synchronization command" in
  Term.(const cmd $ config_t $ listname_t),
  Term.info "sync" ~doc ~sdocs:Manpage.s_common_options ~man

end

module Td_lists = struct

let cmd' = function
  | `Invalid err -> Log.err err
  | `Not_found p -> sprintf "config file '%s' cannot be accessed" p |> Log.err
  | `Ok config ->
  let summary listname =
    let options = Config.options_exn config listname in
    let list_length path = Util.parse_list path |> List.length in
    let todo_length = list_length (Config.get options "todo-path") in
    let done_length = list_length (Config.get options "done-path") in
    sprintf "%s (%d/%d)" listname todo_length done_length
  in
  List.map summary (Config.names config)
  |> String.concat "\n"
  |> print_endline

let cmd config = Util.wrap_error (fun () -> cmd' config)

open Cmdliner

let man = [
  `S Manpage.s_description;
  `P "Summarize the todo lists that are currently maintained. It prints the list
  name as well as the number of undone/done entries. The first name printed
  is the default list.
  To apply a td command to a specific list, run $(b,td [cmd] -l [listname]
  ...)"
]

let term config_t _listname_t =
  let doc = "list all todo lists" in
  Term.(const cmd $ config_t),
  Term.info "lists" ~doc ~sdocs:Manpage.s_common_options ~man

end

open Cmdliner

let config_t =
  let path =
    let doc = "Path to the configuration file. If provided, this argument
    overrides the default path ~/.config/td/config and the environment variable
    $(b,TD_CONFIG)." in
    Arg.(value & opt string (Config.default_path ()) & info ["c"; "config"]
    ~doc ~docv:"CONFIG-FILE")
  in
  Term.(const Config.parse_file $ path)

let listname_t =
  let doc = "Todo list to operate on. Different lists can be managed in
  dedicated sections of the configuration file. See $(b,td init --help) for
  documentation." in
  Arg.(value & opt string "" & info ["l"; "list"] ~doc ~docv:"LIST-NAME")

let man = [
  `S Manpage.s_description;
  `P "Td is a command line tool to manipulate and display todo lists. It can
  create, modify, remove, and selectively print list entries and supports
  context-, project-, and date-tags, allowing flexible filtering and sorting
  operations. All data is stored in a simple human readible text format.";
  `P "To get started, run $(b,td init). This will generate a configuration
  file with default options at $(b,~/.config/td/config) (or at the path pointed
  to by the environment variable $(b,TD_CONFIG)). It will also generate a
  default todo list. Each todo list consists of two files, which are used to
  store active (unfinished) and done (finished) entries. The former is refered
  to as todo-file and the latter as done-file throughout this documentation.
  Alternatively, you can also create a configuration file by hand and run $(b,td
  init) afterwards. This will verify your configuration and touch the specified
  todo- and done-files.";
  `P "Documentation of the configuration options as well as various features,
  including filtering, formatting, and tagging, can be found under $(b,td init
  --help).";
]

let default_cmd =
  let doc = "simple but functional todo list management on the terminal" in
  let sdocs = Manpage.s_common_options in
  let exits = Term.default_exits in
  let default_ls config = Td_ls.cmd config "" `Todo None `Flat "<true>" in
  Term.(const default_ls $ config_t),
  Term.info "td" ~version:_version ~doc ~sdocs ~exits ~man

let () = 
  let commands = [
      Td_lists.term config_t listname_t
    ; Td_sort.term  config_t listname_t
    ; Td_sync.term  config_t listname_t
    ; Td_undo.term  config_t listname_t
    ; Td_init.term  config_t listname_t
    ; Td_mod.term   config_t listname_t
    ; Td_add.term   config_t listname_t
    ; Td_ls.term    config_t listname_t
    ; Td_do.term    config_t listname_t
    ; Td_rm.term    config_t listname_t
  ] in
  Term.(exit @@ eval_choice default_cmd commands)

