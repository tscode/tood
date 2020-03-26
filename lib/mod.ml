open Types

type t =
  | Text of string
  | Prio of Entry.priority
  | Add of Tag.t
  | Del of Tag.t

let text str = Text str
let prio p = Prio p
let add_tag t = Add t
let del_tag t = Del t

let is_text    = function Text _ -> true | _ -> false
let is_prio    = function Prio _ -> true | _ -> false
let is_add_tag = function Add _ -> true | _ -> false
let is_del_tag = function Del _ -> true | _ -> false

let apply m entry = let open Entry in match m with
  | Text text -> { entry with text }
  | Prio prio -> { entry with prio }
  | Del tag   -> 
    let tags = List.filter ~f:Tag.((~=) tag) (tags entry) in
    { entry with tags }
  | Add tag   -> 
    match List.exists ~f:Tag.((=) tag) entry.tags with
    | true  -> entry
    | false -> { entry with tags = tag :: entry.tags }

let apply_list ms entry = List.fold_right ~f:apply ~init:entry ms


module P = struct
  open Parser_base

  let text = quoted >>| text
  let prio = Entry.P.prio >>| prio
  let add fmt_date = Tag.P.parser ~fmt_date ()             >>| add_tag
  let del fmt_date = Tag.P.parser ~fmt_date ~marker:'~' () >>| del_tag

  let parser ?(fmt_date=Date.default_fmt) () =
    choice [ text; prio; add fmt_date; del fmt_date ] 

  let of_string ?fmt_date str =
    match parse_string (only (parser ?fmt_date ())) str with
    | Ok str  -> Ok str
    | Error _ -> Error ("cannot parse modifier '" ^ str ^ "'")

  let parser_list ?fmt_date () = many1 ((parser ?fmt_date ()) <* ws)

  let list_of_string ?fmt_date str =
    match parse_string (only (parser_list ?fmt_date ())) str with
    | Ok str  -> Ok str
    | Error _ -> Error ("cannot parse modifier '" ^ str ^ "'")

end

let to_string ?fmt_date = function
  | Text str  -> "\"" ^ str ^ "\""
  | Prio prio -> Entry.P.prio_to_string prio
  | Add tag   -> Tag.to_string ?fmt_date tag
  | Del tag   -> Tag.P.to_string ?fmt_date ~marker:'~' tag


let of_string = P.of_string
let of_string_exn ?fmt_date str =
  match of_string ?fmt_date str with
  | Ok m    -> m
  | Error err -> raise (ArgumentError err)


let list_of_string = P.list_of_string
let list_of_string_exn ?fmt_date str =
  match list_of_string ?fmt_date str with
  | Ok m      -> m
  | Error err -> raise (ArgumentError err)

