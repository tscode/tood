open Types

include Angstrom

(* Boolean functions on chars *)

let is_digit = function 
  | '0'..'9' -> true
  | _        -> false

let is_symbol_start = function
  | 'a'..'z' | 'A'..'Z' -> true
  | _                   -> false

let is_symbol_char = function
  | 'a'..'z' | 'A'..'Z'
  | '0'..'9' | '-' | '_' -> true
  | _                    -> false 

let is_ws = function 
  | ' ' | '\t' -> true
  | _          -> false

(* Basic parser combinators *)

let ws        = skip_while is_ws
let eoil      = end_of_input <|> end_of_line
let integer   = take_while1 is_digit >>| Int.of_string

let symbol = lift2 String.(^)
  (satisfy is_symbol_start >>| String.of_char)
  (take_while is_symbol_char)

(* Enforcing consumption of the whole string *)

let only p = ws *> p <* ws <* end_of_input

(* Optional parser *)

let opt p = lift Option.some p <|> return None

(* chainl1 infix parser copied from the angstrom README.md *)

let chainl1 expr op =
  let rec go acc =
    (lift2 (fun f x -> f acc x) op expr >>= go) <|> return acc in
  expr >>= go

(* TODO: take_till that only stops if the char is not escaped *)

let take_till_unescaped = take_till

let take_all = take_till (function _ -> false)

let quoted_string =
  let str = take_till_unescaped (function '\'' | '"' -> true | _ -> false) in
  (char '\'' <|> char '"') *> str
