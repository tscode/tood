open Tood

exception KeyError of string
exception ParseError of string

type t = {
    todo_path      : string
  ; done_path      : string
  ; sync_cmd       : string option
  ; entry_fmt      : string
  ; entry_fmt_tree : string
  ; date_fmt       : Date.fmt
} 

let todo_path c = c.todo_path
let done_path c = c.done_path
let sync_cmd c = c.sync_cmd
let entry_fmt c = c.entry_fmt
let entry_fmt_tree c = c.entry_fmt_tree
let date_fmt c = c.date_fmt

let fallback = {
    todo_path      = "./todo.tood"
  ; done_path      = "./done.tood"
  ; sync_cmd       = None
  ; entry_fmt      = "%J %r %t %P_%C_%D"
  ; entry_fmt_tree = "%J %r %t %C_%D"
  ; date_fmt       = Date.fmt_exn "%y-%m-%d"
}

let to_string config = 
  let write key value = key ^ " : " ^ value in
  let maybe key value = match value with
    | Some v -> key ^ " : " ^ v
    | None   -> "# " ^ key ^ " : "
  in
  String.concat ~sep:"\n" [
    write "todo-path" config.todo_path
  ; write "done-path" config.done_path
  ; write "entry-fmt" config.entry_fmt
  ; write "entry-fmt-tree" config.entry_fmt_tree
  ; write "date-fmt" (Date.fmt_to_string config.date_fmt)
  ; maybe "sync-cmd" config.sync_cmd
]

let add config (key, value) = match Symbol.to_string key with
  | "todo-path"      -> { config with todo_path = value }
  | "done-path"      -> { config with done_path = value }
  | "sync-cmd"       -> { config with sync_cmd  = Some value }
  | "date-fmt"       -> { config with date_fmt  = Date.fmt_exn value }
  | "entry-fmt"      -> { config with entry_fmt = value }
  | "entry-fmt-tree" -> { config with entry_fmt_tree = value }
  | _ -> raise (KeyError (Symbol.to_string key))

let parse path =  
  let open Angstrom in
  let open Parser in
  let comment = char '#' *> take_all *> return () in
  let ignore  = (ws *> comment <|> only ws) *> return None in
  let key     = ws *> symbol <* ws <* char ':' <* ws in
  let value   = take_while Char.((<>) '#') >>| String.strip in
  let tup a b = Some (a, b) in
  let parser  = lift2 tup key (value <* opt comment) <|> ignore in
  let f line  = match parse_string (only parser) line with
    | Ok tup  -> tup
    | Error _ -> raise (ParseError line)
  in
  try List.filter_map ~f (In_channel.read_lines path)
  |> List.fold ~init:fallback ~f:add |> Result.return
  with
  | Sys_error _  -> Error (`Not_found path)
  | KeyError key -> Error 
    (`Invalid ("invalid key '" ^ key ^ "' in config file " ^ path))
  | ParseError l -> Error 
    (`Invalid ("cannot parse line '" ^ l ^ "' in config file " ^ path))

