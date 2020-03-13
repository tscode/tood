open Types

type t =
  | Context of symbol
  | Project of symbol list
  | Due     of Date.t

let context sym = Context sym
let project sym = Project sym
let due date    = Due date

let compare t t' = match t, t' with
  | Context str, Context str' -> String.compare str str'
  | Project pr, Project pr'   -> List.compare String.compare pr pr'
  | Due d, Due d'             -> Date.compare d d' 
  | Context _, Project _      -> -1
  | Project _, Due _          -> -1
  | Context _, Due _          -> -1
  | Project _, Context _      ->  1
  | Due _, Context _          ->  1
  | Due _, Project _          ->  1

let (=) a b = compare a b = 0
let (~=) a b = not (a = b)

(* Module for parsing / reading related functions *)

module P = struct
  open Parser

  (* Syntax *)

  let tagmarker   = '+'
  let project_sep = '/'

  (* Writing *)

  let project_to_string = function
    | []  -> ""
    | [p] -> p ^ String.of_char project_sep
    | ts  -> String.concat ~sep:(String.of_char project_sep) ts

  let to_string ?(marker=tagmarker) = 
    let m = String.of_char tagmarker in function
      | Context t  -> m ^ t
      | Project ts -> m ^ project_to_string ts
      | Due date   -> m ^ Date.to_string date

  (* Reading *)

  let context_tag = symbol
  let date_tag    = Date.P.parser

  let project_tag =
    lift2 List.cons
      (symbol <* char project_sep)
      (sep_by (char project_sep) symbol)

  let parser ?(marker=tagmarker) date_parser =
        (char marker *> project_tag >>| project)
    <|> (char marker *> context_tag >>| context)
    <|> (char marker *> date_parser >>| due)

  let of_string ?(fmt_date=Date.default_fmt) str =
    let f _ date_parser = parse_string (only (parser date_parser)) str in
    Date.with_fmt ~fmt:fmt_date ~f

end

let to_string = P.to_string ~marker:P.tagmarker
let of_string = P.of_string

let of_string_exn ?fmt_date str = match of_string ?fmt_date str with
  | Ok tag -> tag
  | Error _ -> raise (ArgumentError ("invalid tag '" ^ str ^ "'"))

