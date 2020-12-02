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

type part =
  | Index
  | Prio
  | Text
  | Tag
  | Project
  | Context
  | Date

type t = {
    text : string
  ; tags : Tag.t list
  ; prio : priority
}

let create ?(tags=[]) ?(prio=Default) text =
  let tags = Tag.(List.sort_uniq compare tags) in
  {text; tags; prio}

let text entry = entry.text
let tags entry = entry.tags
let prio entry = entry.prio

let context_tags entry = 
  let f = function
    | Tag.Context c -> Some c
    | _ -> None
  in
  List.filter_map f entry.tags

let project_tags entry =
  let f = function
    | Tag.Project ps -> Some ps
    | _ -> None
  in
  List.filter_map f entry.tags

let due_tags entry =
  let f = function
    | Tag.Due date -> Some date
    | _ -> None
  in
  List.filter_map f entry.tags

(* Indexed entry lists *)
let index l = List.mapi (fun i entry -> (i+1, entry)) l

let index_with indices l = 
  match List.map2 (fun i entry -> i, entry) indices l with
  | l -> Result.Ok l
  | exception _ -> Result.Error "list of indices has wrong length"

let deindex l = List.map snd l
let indices l = List.map fst l

let max_index l =
  let max i j = if i < j then j else i in
  match List.map fst l |> List.fold_left max (-1) with
  | -1 -> None
  | idx -> Some idx

let min_index =
  let min i j = if i < j then i else j in function
  | [] -> None
  | h :: t -> List.map fst t |> List.fold_left min (fst h) |> Option.some

(* Module for parsing / writing related functions *)

module P = struct
  open Parser_base

  (* Default entry format string *)

  let default_layout = "%r %s %p_%c_%d"

  let is_prio_char = function
    | '!' | '-' | '?' -> true
    | _               -> false

  (* Writing entries *)

  let prio_to_string = function
    | High    -> "!"
    | Default -> "-"
    | Low     -> "?"

  let tags fmt entry = entry.tags |> List.map (Tag.to_string ~fmt)
  let ptags entry = project_tags entry |> List.map Tag.P.project_to_string
  let dtags fmt entry = due_tags entry |> List.map (Date.P.to_string ~fmt)

  let collect sep marks tags = match marks with
    | false -> String.concat sep tags
    | true ->
      let add_mark = (^) (Char.escaped Tag.P.tagmarker) in
      List.map add_mark tags |> String.concat sep

  let maybe_add_sep sep = function
    | false -> Fun.id
    | true -> function "" -> "" | s -> s ^ sep

  let substitute ?(style = fun _ _ s -> s) sep fmt entry =
    let lookup ph postfix =
      let adapt = maybe_add_sep sep (postfix = Some '_') in
      let ph_lower = Char.lowercase_ascii ph in
      let marks = (ph_lower = ph) in
      let stl = style entry in
      match ph_lower with
      | 'r' -> prio_to_string entry.prio |> stl Prio
      | 's' -> entry.text |> adapt |> stl Text
      | 't' -> collect sep marks (tags fmt entry) |> adapt |> stl Tag
      | 'c' -> collect sep marks (context_tags entry) |> adapt |> stl Context
      | 'p' -> collect sep marks (ptags entry) |> adapt |> stl Project
      | 'd' -> collect sep marks (dtags fmt entry) |> adapt |> stl Date
      | _ -> assert false
    in
    let is_ph = function
    | 'r' | 's' | 't' | 'T' | 'c' | 'C' | 'p' | 'P' | 'd' | 'D' -> true
    | _ -> false
    in
    let is_postfix = function '_' -> true | _ -> false in
    sub_placeholders '%' is_ph is_postfix lookup

  let format ?style ?(fmt=Date.default_fmt) ?(sep=" ") layout entry =
    let p = substitute ?style sep fmt entry in
    parse_string ~consume:All p layout |> Result.get_ok

  let to_string ?fmt entry = format ?fmt default_layout entry

  let to_string_strict entry =
    String.concat " " [
        prio_to_string entry.prio
      ; entry.text
      ; List.map Tag.to_string entry.tags |> String.concat " "
    ]

  let format_indexed ?(style = fun _ _ s -> s)
                     ?fmt
                     ?(max_index=(-1))
                     ?(sep=" ") layout (index, entry) =
    let padding () = 
      let chars i = Int.to_string i |> String.length in
      let max_index = if max_index > index then max_index else index in
      String.make (chars max_index - chars index) ' '
    in
    let stl = style entry Index in
    let lookup ph _ = match ph with
      | 'i' -> Int.to_string index |> stl
      | 'I' -> Int.to_string index ^ padding () |> stl
      | 'J' -> padding () ^ Int.to_string index  |> stl
      | _ -> assert false
    in
    let is_ph = function 'i' | 'I' | 'J' -> true | _ -> false in
    let p = sub_placeholders '%' is_ph (Fun.const false) lookup in
    let str = format ~style ?fmt ~sep layout entry in
    parse_string ~consume:All p str |> Result.get_ok


  (* Reading *)

  let prio =
    satisfy is_prio_char >>| function
    | '!' -> High
    | '-' -> Default
    | '?' -> Low
    | _   -> raise ImpossibleError

  let text = take_till ((=) Tag.P.tagmarker) >>| String.trim

  let parser_strict =
    let f prio text tags = create ~tags ~prio text in
    lift3 f (prio <* ws) text (many (ws *> Tag.P.parser ()))


  let of_string_strict str =
  match parse_string ~consume:All (only parser_strict) str with
  | Ok entry -> Ok entry
  | Error _  -> Error ("cannot parse entry '" ^ str ^ "'")

  let parser ?fmt () = 
    let f prio text tags = create ~tags ~prio text in
    let tags_parser = many (ws *> Tag.P.parser ?fmt ()) in
    lift3 f (option Default (prio <* ws)) text tags_parser

  let of_string ?fmt str =
    let entry_parser = parser ?fmt () in
    match parse_string ~consume:All (only entry_parser) str with
    | Ok entry -> Ok entry
    | Error _  -> Error ("cannot understand entry '" ^ str ^ "'")

end

(* Bring parsing related functions in main Entry module *)

let default_layout = P.default_layout

let to_string = P.to_string
let of_string = P.of_string

let to_string_strict = P.to_string_strict
let of_string_strict = P.of_string_strict

let of_string_exn ?fmt str =
  match of_string ?fmt str with
    | Ok entry  -> entry
    | Error err -> raise (ArgumentError err)

let of_string_strict_exn str =
  match of_string_strict str with
    | Ok entry  -> entry
    | Error err -> raise (ArgumentError err)

let format         = P.format
let format_indexed = P.format_indexed

(* Sorting *)

let compare_lists compare l l' = match l, l' with
  | [], [] -> 0
  | _, [] -> 1
  | [], _ -> -1
  | h :: _, h' :: _ -> compare h h'

let compare_by_part part (i, entry) (i', entry') =
  match part with
  | Index -> compare i i'
  | Prio  -> compare (prio entry) (prio entry')
  | Text  -> compare (text entry) (text entry')
  | Date  -> compare_lists Date.compare (due_tags entry) (due_tags entry')
  | Tag -> compare_lists compare (tags entry) (tags entry')
  | Context -> compare_lists compare (context_tags entry) (context_tags entry')
  | Project ->
    let compare l l' = compare_lists compare l l' in
    compare_lists compare (project_tags entry) (project_tags entry')

let rec compare_by_parts parts x y = match parts with
  | [] -> 0
  | (part, invert) :: t ->
  match compare_by_part part x y with
  | 0 -> compare_by_parts t x y
  | value -> if invert then (-value) else value

let sort_indexed parts = List.sort (compare_by_parts parts)

let sort parts l =
  List.map (fun entry -> (-1, entry)) l
  |> sort_indexed parts
  |> List.map snd

