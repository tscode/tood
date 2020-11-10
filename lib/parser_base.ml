
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
let integer   = take_while1 is_digit >>| int_of_string


let symbol = lift2 (^)
  (satisfy is_symbol_start >>| Char.escaped)
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

let take_till_unescaped_ f =
  let escaped = char '\\' *> satisfy f >>| Char.escaped in
  let bslash = char '\\' >>| Char.escaped in
  let block = take_till (fun c -> f c || c = '\\') in
  lift (String.concat "") (many (escaped <|> bslash <|> block))

let take_till_unescaped_char c = take_till_unescaped ((=) c)

let take_all = take_till (function _ -> false)

let quoted c = char c *> take_till_unescaped ((=) c) <* char c

(* Parser that substitutes placeholders in a string.
 * The placeholder has to consist of a prefix char, like '%', the placeholder
 * char itself, like 'd', and an (optional) postfix char, like "_". A lookup
 * function is used to resolve each valid placholder *)

let fail_at_eoi p = p >>= function
  | "" -> peek_char_fail *> return ""
  | str -> return str

let debug head str =
  print_endline (head ^ ": '" ^ str ^ "'"); str

let sub_placeholders prefix is_ph is_postfix lookup =
  let ph = lift2 lookup (satisfy is_ph) (opt (satisfy is_postfix)) in
  let part = choice
    [ (char prefix *> (ph <|> return (Char.escaped prefix)))
    ; fail_at_eoi (take_till_unescaped_char prefix) ]
  in
  many part >>| String.concat ""

