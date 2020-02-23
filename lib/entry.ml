open Types

type priority =
  | High
  | Default
  | Low

let prio_to_int = function
  | High    -> 1
  | Default -> 0
  | Low     -> -1

let prio_of_int = function
  | 0 -> Default
  | a when a > 0 -> High
  | a when a < 0 -> Low
  | _ -> raise ImpossibleError

type t = {
    text : symbol
  ; tags : Tag.t list
  ; prio : priority
}

let create ?(tags=[]) ?(prio=Default) text = {text; tags; prio}

let text entry = entry.text
let tags entry = entry.tags
let prio entry = entry.prio

let context_tags entry = 
  let f = function
    | Tag.Context c -> Some c
    | _ -> None
  in
  List.filter_map ~f entry.tags

let project_tags entry =
  let f = function
    | Tag.Project ps -> Some ps
    | _ -> None
  in
  List.filter_map ~f entry.tags

let due_tags entry =
  let f = function
    | Tag.Due date -> Some date
    | _ -> None
  in
  List.filter_map ~f entry.tags

(* Module for parsing / reading related functions *)

module P = struct
  open Parser

  (* Syntax *)

  let is_prio_char = function
    | '!' | '-' | '?' -> true
    | _               -> false

  (* Writing *)

  let prio_to_string = function
    | High    -> "!"
    | Default -> "-"
    | Low     -> "?"

  let to_string entry =
    String.concat ~sep:" " [
       prio_to_string entry.prio
     ; entry.text
     ; List.map ~f:Tag.to_string entry.tags |> String.concat ~sep:" "
    ]

  (* Reading *)

  let prio =
    satisfy is_prio_char >>| function
      | '!' -> High
      | '-' -> Default
      | '?' -> Low
      | _   -> raise ImpossibleError

  let text = take_till (fun c ->
    Char.(c = '\n' || c = Tag.P.tagmarker))
    >>| String.strip

  let parser =
    let f prio text tags = {text; tags; prio} in
    Parser.(lift3 f (prio <* ws) text (many (ws *> Tag.P.parser)))

  let parser_relaxed =
    let f prio text tags = {text; tags; prio} in
    Parser.(lift3 f (option Default (prio <* ws)) text (many Tag.P.parser))

  let of_string = parse_string (only parser)
  let of_string_relaxed = parse_string (only parser_relaxed)

end

let to_string = P.to_string
let of_string = P.of_string
let of_string_relaxed = P.of_string_relaxed

let of_string_exn str =
  match of_string str with
    | Ok entry -> entry
    | Error err -> raise (ArgumentError err)

let of_string_relaxed_exn str =
  match of_string_relaxed str with
    | Ok entry -> entry
    | Error err -> raise (ArgumentError err)

