open Types

type t =
  | Context of symbol
  | Project of symbol list
  | Due     of Date.t

let context sym = Context sym
let project sym = Project sym
let due date    = Due date

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

  let to_string = 
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

  let parser =
        (char tagmarker *> project_tag >>| project)
    <|> (char tagmarker *> context_tag >>| context)
    <|> (char tagmarker *> date_tag    >>| due)

  let of_string = parse_string (only parser) 

end

let to_string = P.to_string
let of_string = P.of_string

let of_string_exn str = match of_string str with
  | Ok tag -> tag
  | Error _ -> raise (ArgumentError ("invalid tag '" ^ str ^ "'"))

