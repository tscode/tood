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

let apply m entry = match m with
  | Text text -> { entry with Entry.text = text }
  | Prio prio -> { entry with Entry.prio = prio }
  | Del tag   -> 
    let tags = List.filter Tag.((~=) tag) (Entry.tags entry) in
    { entry with Entry.tags = tags }
  | Add tag   -> 
    match List.exists Tag.((=) tag) entry.Entry.tags with
    | true  -> entry
    | false -> { entry with Entry.tags = tag :: entry.tags }

let apply_list ms entry = List.fold_right apply ms entry


module P = struct
  open Parser_base

  let text = lift text (quoted '\'' <|> quoted '"')
  let prio = Entry.P.prio >>| prio
  let add fmt = Tag.P.parser ~fmt ()             >>| add_tag
  let del fmt = Tag.P.parser ~fmt ~marker:'~' () >>| del_tag

  let parser ?(fmt=Date.default_fmt) () =
    choice [ text; prio; add fmt; del fmt ] 

  let of_string ?fmt str =
    match parse_string ~consume:All (only (parser ?fmt ())) str with
    | Ok str  -> Ok str
    | Error _ -> Error ("cannot parse modifier '" ^ str ^ "'")

  let parser_list ?fmt () = many1 ((parser ?fmt ()) <* ws)

  let list_of_string ?fmt str =
    match parse_string ~consume:All (only (parser_list ?fmt ())) str with
    | Ok str  -> Ok str
    | Error _ -> Error ("cannot parse modifier '" ^ str ^ "'")

end

let to_string ?fmt = function
  | Text str  -> "\"" ^ str ^ "\""
  | Prio prio -> Entry.P.prio_to_string prio
  | Add tag   -> Tag.to_string ?fmt tag
  | Del tag   -> Tag.P.to_string ?fmt ~marker:'~' tag


let of_string = P.of_string
let of_string_exn ?fmt str =
  match of_string ?fmt str with
  | Ok m    -> m
  | Error err -> raise (ArgumentError err)


let list_of_string = P.list_of_string
let list_of_string_exn ?fmt str =
  match list_of_string ?fmt str with
  | Ok m      -> m
  | Error err -> raise (ArgumentError err)

