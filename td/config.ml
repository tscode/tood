open Tood
open Printf

module Dict = Map.Make(String)

exception ParseError of string
exception KeyError of string
exception ValueError of string

(* Configuration options for a todo list *)
type options = string Dict.t

let nooptions = Dict.empty

let fallback_options =
  Dict.empty
  |> Dict.add "todo-path" "./todo.txt"
  |> Dict.add "done-path" "./done.txt"
  |> Dict.add "entry-fmt" "%J %r %s %p_%c_%d"
  |> Dict.add "entry-fmt-tree" "%J %r %s %c_%d"
  |> Dict.add "date-fmt" "%y-%m-%d"
  |> Dict.add "editor" "vi"
  |> Dict.add "sync-cmd" ""
  |> Dict.add "index-style" ""
  |> Dict.add "prio-style" ""
  |> Dict.add "text-style" ""
  |> Dict.add "tag-style" ""
  |> Dict.add "project-style" ""
  |> Dict.add "context-style" ""
  |> Dict.add "date-style" ""

let fallback =
  [ "", fallback_options
  ; "default", Dict.empty ]

let default ?(listname="default") todo_path done_path =
  let general = fallback_options
    |> Dict.remove "todo-path"
    |> Dict.remove "done-path"
  in
  let default = Dict.empty
    |> Dict.add "todo-path" todo_path
    |> Dict.add "done-path" done_path
  in
  [ "", general
  ; listname, default ]

let get_safe config key =
  match Dict.find_opt key config with
  | None | Some "" -> None
  | opt -> opt

let get config key =
  get_safe config key |> Option.get

let merge fallback config =
  let merger _key a = function
  | None | Some "" -> a
  | opt -> opt
  in
  Dict.merge merger fallback config

(* collection of named configurations for different todo lists *)
type t = (string * options) list

let names config =
  List.map fst config |> List.tl

let options config name =
  match config with
  | [] -> assert false
  | general :: config -> 
    match List.assoc_opt name config with
    | None -> None
    | Some options ->
      merge (snd general) options
      |> merge fallback_options
      |> Option.some

let options_exn config name =
  options config name |> Option.get

let to_string config = 
  let append key value l =
    let line = match value with
    | "" -> "# " ^ key ^ " : "
    | v -> key ^ " : " ^ v
    in
    line :: l
  in
  let collect_options (name, options) =
    Dict.fold append options [] |> String.concat "\n" |> (^) (name ^ "\n\n")
  in
  List.map collect_options config |> String.concat "\n\n"

type style =
  { normal : string list
  ; low    : string list
  ; high   : string list }

let style_parser =
  let open Angstrom in
  let open Parser in
  let word = take_while1 is_symbol_char in
  let words = many (ws *> word <* ws) in
  let normal = words >>| (fun s -> { normal = s; low = s; high = s}) in
  let low = char '?' *> words >>| (fun l s -> { s with low = l }) in
  let high = char '!' *> words >>| (fun h s -> { s with high = h}) in
  lift2 (List.fold_left (fun s f -> f s)) normal (many (low <|> high))

let printer_exn str ?(prio=Entry.Default) =
  match Angstrom.(parse_string ~consume:Consume.All style_parser str) with
  | Error _ -> raise (ArgumentError str)
  | Ok style ->
  let codes = match prio with
  | Entry.High -> List.map Style.parse_code style.high
  | Entry.Low  -> List.map Style.parse_code style.low
  | _   -> List.map Style.parse_code style.normal
  in
  Style.apply codes

let printer str =
  match printer_exn str with
  | style -> Ok style
  | exception (ArgumentError err) -> Error err

let get_printer options key =
  printer_exn (get_safe options key |> Option.value ~default:"")

let get_printer_safe options key =
  match get_safe options key with
  | None -> Some (printer_exn "")
  | Some style -> printer style |> Result.to_option

let sanity_tests =
  let always_ok =
    Fun.const None
  in
  let test_result f key value =
    if value = "" then None else
    match f value with
    | Ok _ -> None
    | Error _ ->
      Some (sprintf "invalid value '%s' for key '%s'" value key)
  in
  [ "todo-path",      always_ok
  ; "done-path",      always_ok
  ; "entry-fmt",      always_ok
  ; "entry-fmt-tree", always_ok
  ; "date-fmt",       test_result Date.fmt "date-fmt"
  ; "sync-cmd",       always_ok
  ; "editor",         always_ok
  ; "index-style",    test_result printer "index-style"
  ; "prio-style",     test_result printer "prio-style"
  ; "text-style",     test_result printer "text-style"
  ; "tag-style",      test_result printer "tag-style"
  ; "project-style",  test_result printer "project-style"
  ; "context-style",  test_result printer "context-style"
  ; "date-style",     test_result printer "date-style"
  ]

let valid_option_keys = List.map fst sanity_tests

let add options key value =
  match List.assoc_opt key sanity_tests with 
  | None -> Error (sprintf "'%s' is not a valid option key" key)
  | Some test -> 
  match test value with
  | Some err -> Error (sprintf "value for option key '%s' not valid: '%s'" key err)
  | None -> Ok (Dict.add key value options)

let check_option name value =
  match List.assoc_opt name sanity_tests with
  | None -> `KeyError name
  | Some f ->
  match f value with
  | Some err -> `ValueError err
  | None -> `Ok

let check_option_exn name value =
  match check_option name value with
  | `Ok -> ()
  | `KeyError key -> raise (KeyError key)
  | `ValueError msg -> raise (ValueError msg)

let check config =
  let check_options (_name, options) =
    Dict.to_seq options |> Seq.iter (fun (x,y) -> check_option_exn x y)
  in
  List.iter check_options config; config

let default_path () =
  match Sys.getenv_opt "TD_CONFIG" with
  | Some path -> path
  | None      -> "~/.config/td/config"

let parse_options strs =
  let open Angstrom in
  let open Parser in
  let comment = char '#' *> take_all *> return () in
  let key = symbol <* ws <* char ':' >>| Symbol.to_string in
  let value = take_till_unescaped_char '#' <* opt comment >>| String.trim in
  let parser = lift2 (fun key value -> (key, value)) key value in
  let f options str = match parse_string ~consume:All parser str with
    | Error _ -> raise (ParseError (sprintf "cannot parse option '%s'" str))
    | Ok (key, value) -> match add options key value with
      | Ok options -> options
      | Error err -> raise (ParseError err)
  in
  match List.fold_left f Dict.empty strs with
  | options -> `Ok options
  | exception ParseError msg -> `Invalid msg

let parse_file path =
  let open Angstrom in
  let open Parser in
  let keysym = symbol >>| Symbol.to_string in
  let comment = char '#' *> take_all *> return () in
  let lstname = ws *> keysym <* ws <* opt comment in
  let pass lst = ws *> comment <|> only ws >>| Fun.const lst in
  let push lst =
    lift2 (fun key value -> fst lst, Dict.add key value (snd lst))
      (ws *> keysym <* ws <* char ':')
      (take_till_unescaped_char '#' <* opt comment >>| String.trim)
  in
  let rec parse_lines config active = function
    | [] -> active :: config |> List.rev
    | line :: lines ->
    match parse_string ~consume:All (pass active <|> push active) line with
    | Ok lst -> parse_lines config lst lines
    | Error _ ->
    match parse_string ~consume:All lstname line with
    | Ok name -> parse_lines (active :: config) (name, Dict.empty) lines
    | Error _ -> raise (ParseError line)
  in
  let err_msg = sprintf "cannot parse config file %s: %s" path in
  try
    match parse_lines [] ("", Dict.empty) (Io.read_lines path) with
    | [] -> assert false
    | h :: [] -> `Ok (h :: [("default", Dict.empty)] |> check)
    | config -> `Ok (config |> check)
  with
  | Sys_error _    -> `Not_found path
  | ValueError msg -> `Invalid (err_msg msg)
  | KeyError key   -> `Invalid (err_msg (sprintf "invalid key '%s'" key))
  | ParseError err -> `Invalid (err_msg (sprintf "invalid line '%s'" err))

