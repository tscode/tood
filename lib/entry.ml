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

type fmt = string

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
  open Parser

  (* Default entry format string *)

  let default_fmt = "%r %t %P_%C_%D"

  let is_prio_char = function
    | '!' | '-' | '?' -> true
    | _               -> false

  (* Writing *)

  let prio_to_string = function
    | High    -> "!"
    | Default -> "-"
    | Low     -> "?"

  (* 
   * What follows here is probably very inefficient. I should benchmark and
   * possibly change it.
   * All of this is probably much faster and nicer with an Angstrom parser
   *)

  let format ~fmt ?(fmt_date=Date.default_fmt) ?(strip=true) ?(tag_sep=" ") entry =
    let prepend_tag = (^) (String.of_char Tag.P.tagmarker) in
    let collect = String.concat ~sep:tag_sep in
    let ctags   = context_tags entry in
    let ptags   = project_tags entry |> List.map ~f:Tag.P.project_to_string in
    let dtags   = due_tags entry     |> List.map ~f:(Date.P.format ~fmt:fmt_date) in
    let sep     = tag_sep in 
    let open Sub in 
    let output = fmt
    |> sub ~sep (t, entry.text)
    |> sub ~sep (r, prio_to_string entry.prio)
    |> sub ~sep (p, collect ptags)
    |> sub ~sep (c, collect ctags)
    |> sub ~sep (d, collect dtags)
    |> sub ~sep (p', List.map ~f:prepend_tag ptags |> collect)
    |> sub ~sep (c', List.map ~f:prepend_tag ctags |> collect)
    |> sub ~sep (d', List.map ~f:prepend_tag dtags |> collect)
    in if strip then String.strip output else output

    (*

  let substitute sep pat value str =
    let open String.Search_pattern in
    let value_ = if String.(value = "") then value else value ^ sep in
    let str = replace_all (create (pat ^ "_")) ~in_:str ~with_:value_ in
    replace_all (create pat) ~in_:str ~with_:value

  let format ~fmt ~fmt_date ?(tag_sep=" ") entry = 
    let prepend_tag = (^) (String.of_char Tag.P.tagmarker) in
    let collect = String.concat ~sep:tag_sep in
    let ctags = context_tags entry in
    let ptags = project_tags entry |> List.map ~f:Tag.P.project_to_string in
    let dtags = due_tags entry |> List.map ~f:(Date.P.format ~fmt:fmt_date)
    in fmt 
    |> substitute tag_sep "%r" (prio_to_string entry.prio)
    |> substitute tag_sep "%t" entry.text
    |> substitute tag_sep "%p" (collect ptags)
    |> substitute tag_sep "%c" (collect ctags)
    |> substitute tag_sep "%d" (collect dtags)
    |> substitute tag_sep "%P" (collect (List.map ~f:prepend_tag ptags))
    |> substitute tag_sep "%C" (collect (List.map ~f:prepend_tag ctags))
    |> substitute tag_sep "%D" (collect (List.map ~f:prepend_tag dtags))

    *)

  (* Should also work but will probably be much slower? 
   * TODO: Benchmark this!
   * let to_string entry = format ~fmt ~fmt_date:Date.fmt entry *)

  let to_string entry =
    String.concat ~sep:" " [
       prio_to_string entry.prio
     ; entry.text
     ; List.map ~f:Tag.to_string entry.tags |> String.concat ~sep:" "
    ]

  let number_of_chars i = Int.to_string i |> String.length

  let format_index ~fmt ~fmt_date ~max_index ?(tag_sep=" ") (index, entry) =
    let index_str = Int.to_string index in
    let padding =
      let l = number_of_chars max_index - number_of_chars index in
      String.make (Int.max 0 l) ' '
    in
    let sep = tag_sep in
    let open Sub in
    format ~fmt ~fmt_date ~tag_sep entry
    |> sub ~sep (i,  index_str)
    |> sub ~sep (i', index_str ^ padding)
    |> sub ~sep (j', padding ^ index_str)


  (* Reading *)

  let prio =
    satisfy is_prio_char >>| function
      | '!' -> High
      | '-' -> Default
      | '?' -> Low
      | _   -> raise ImpossibleError

  let text = take_till (fun c -> Char.(c = Tag.P.tagmarker)) >>| String.strip

  let parser =
    let f prio text tags = {text; tags; prio} in
    Parser.(lift3 f (prio <* ws) text (many (ws *> Tag.P.parser)))

  let parser_relaxed =
    let f prio text tags = {text; tags; prio} in
    Parser.(
      lift3 f (option Default (prio <* ws)) text (many (ws *> Tag.P.parser))
    )

  let of_string = parse_string (only parser)
  let of_string_relaxed = parse_string (only parser_relaxed)

end

(* Bring parsing related functions in main Entry module *)

let fmt a = a
let fmt_to_string a = a
let default_fmt = P.default_fmt

let format = P.format
let format_index = P.format_index
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

