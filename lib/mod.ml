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
  open Parser

  let text = quoted_string >>| text
  let prio = Entry.P.prio >>| prio
  let add date_parser = Tag.P.parser date_parser >>| add_tag
  let del date_parser = Tag.P.parser ~marker:'~' date_parser >>| del_tag

  let parser date_parser =
    choice [ text; prio; add date_parser; del date_parser ] 

  let of_string ?(fmt_date=Date.default_fmt) str =
    let f _ date_parser = 
      match parse_string (only (parser date_parser)) str with
      | Ok str  -> Ok str
      | Error _ -> Error ("cannot parse modifier '" ^ str ^ "'")
    in
    Date.with_fmt ~fmt:fmt_date ~f

  let list_parser date_parser = many1 ((parser date_parser) <* ws)

  let list_of_string ?(fmt_date=Date.default_fmt) str =
    let f _ date_parser =
      match parse_string (only (list_parser date_parser)) str with
      | Ok str  -> Ok str
      | Error _ -> Error ("cannot parse modifier '" ^ str ^ "'")
    in
    Date.with_fmt ~fmt:fmt_date ~f

end

let to_string = function
  | Text str  -> "\"" ^ str ^ "\""
  | Prio prio -> Entry.P.prio_to_string prio
  | Add tag   -> Tag.to_string tag
  | Del tag   -> Tag.P.to_string ~marker:'~' tag


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

