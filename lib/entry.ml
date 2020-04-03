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
    text : string
  ; tags : Tag.t list
  ; prio : priority
}

let create ?(tags=[]) ?(prio=Default) text =
  let tags = Tag.(List.dedup_and_sort ~compare tags) in
  {text; tags; prio}

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

(* Indexed entry lists *)
let index l = List.mapi l ~f:(fun i entry -> (i+1, entry))

let index_with indices l = 
  let open List.Or_unequal_lengths in
  match List.map2 indices l ~f:(fun i entry -> i, entry) with
  | Ok l -> Result.Ok l
  | Unequal_lengths -> Result.Error "list of indices has wrong length"

let deindex l = List.map ~f:snd l
let indices l = List.map ~f:fst l

let min_index l = List.min_elt ~compare:Int.compare (indices l)
let max_index l = List.max_elt ~compare:Int.compare (indices l)

(* Module for parsing / writing related functions *)

module P = struct
  open Parser_base

  (* Default entry format string *)

  let default_fmt_string = "%r %t %P_%C_%D"

  let is_prio_char = function
    | '!' | '-' | '?' -> true
    | _               -> false

  (* Writing *)

  let prio_to_string = function
    | High    -> "!"
    | Default -> "-"
    | Low     -> "?"

  (* 
   * What follows here is probably inefficient. I should benchmark and
   * possibly change it.
   * All of this is probably much faster with an Angstrom parser ?
   *)

  let format ?(fmt_date=Date.default_fmt) ?(rstrip=true) ?(tag_sep=" ") fmt_str entry =
    let prepend_tag = (^) (String.of_char Tag.P.tagmarker) in
    let collect = String.concat ~sep:tag_sep in
    let ctags   = context_tags entry in
    let ptags   = project_tags entry |> List.map ~f:Tag.P.project_to_string in
    let dtags   = due_tags entry |> List.map ~f:(Date.P.to_string ~fmt:fmt_date) in
    let sep     = tag_sep in 
    let open Sub in 
    let output = fmt_str
    |> sub ~sep (t, entry.text)
    |> sub ~sep (r, prio_to_string entry.prio)
    |> sub ~sep (p, collect ptags)
    |> sub ~sep (c, collect ctags)
    |> sub ~sep (d_, collect dtags)
    |> sub ~sep (p', List.map ~f:prepend_tag ptags |> collect)
    |> sub ~sep (c', List.map ~f:prepend_tag ctags |> collect)
    |> sub ~sep (d', List.map ~f:prepend_tag dtags |> collect)
    in if rstrip then String.rstrip output else output


  let to_string ?fmt_date entry = format ?fmt_date default_fmt_string entry

  let to_string_strict entry =
    String.concat ~sep:" " [
        prio_to_string entry.prio
      ; entry.text
      ; List.map ~f:Tag.to_string entry.tags |> String.concat ~sep:" "
    ]

  let number_of_chars i = Int.to_string i |> String.length

  let format_index ?fmt_date ?max_index ?(rstrip=true)
                   ?(tag_sep=" ") fmt_string (index, entry) =
    let index_str = Int.to_string index in
    let max_index = Option.value ~default:index max_index in
    let padding =
      let l = number_of_chars max_index - number_of_chars index in
      String.make (Int.max 0 l) ' '
    in
    let sep = tag_sep in
    let open Sub in
    let output = format ?fmt_date ~tag_sep ~rstrip:false fmt_string entry
    |> sub ~sep (i,  index_str)
    |> sub ~sep (i', index_str ^ padding)
    |> sub ~sep (j', padding ^ index_str)
    in if rstrip then String.rstrip output else output


  (* Reading *)

  let prio =
    satisfy is_prio_char >>| function
    | '!' -> High
    | '-' -> Default
    | '?' -> Low
    | _   -> raise ImpossibleError

  let text = take_till (fun c -> Char.(c = Tag.P.tagmarker)) >>| String.strip

  let parser_strict =
    let f prio text tags = create ~tags ~prio text in
    lift3 f (prio <* ws) text (many (ws *> Tag.P.parser ()))

  let of_string_strict str = match parse_string (only parser_strict) str with
  | Ok entry -> Ok entry
  | Error _  -> Error ("cannot parse entry '" ^ str ^ "'")

  let parser ?fmt_date () = 
    let f prio text tags = create ~tags ~prio text in
    let tags_parser = many (ws *> Tag.P.(parser ?fmt_date ())) in
    lift3 f (option Default (prio <* ws)) text tags_parser

  let of_string ?fmt_date str =
    let entry_parser = parser ?fmt_date () in
    match parse_string (only entry_parser) str with
    | Ok entry -> Ok entry
    | Error _  -> Error ("cannot understand entry '" ^ str ^ "'")

end

(* Bring parsing related functions in main Entry module *)

let default_fmt_string = P.default_fmt_string

let to_string = P.to_string
let of_string = P.of_string

let to_string_strict = P.to_string_strict
let of_string_strict = P.of_string_strict

let of_string_exn ?fmt_date str =
  match of_string ?fmt_date str with
    | Ok entry  -> entry
    | Error err -> raise (ArgumentError err)

let of_string_strict_exn str =
  match of_string_strict str with
    | Ok entry  -> entry
    | Error err -> raise (ArgumentError err)

let format       = P.format
let format_index = P.format_index

