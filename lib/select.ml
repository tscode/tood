open Types

type pat = string

type t =
  | And      of t * t
  | Or       of t * t
  | Not      of t
  | Index    of int
  | Text     of pat
  | Project  of pat list
  | Context  of pat
  | Priority of Entry.priority
  | Due      of (Date.t option * Date.t option)
  | True
  | False

let and' a b  = And (a, b) 
let or'  a b  = Or (a, b)
let not' a    = Not a

let index idx = Index idx
let text p    = Text p
let project p = Project p
let context p = Context p
let prio a    = Priority a
let due a     = Due (Some a, Some a)

let due_range a b = match (a, b) with
  | _, None | None, _ -> Due (a, b)
  | Some a, Some b -> match Date.(a <= b) with
    | true  -> Due (Some a, Some b)
    | false -> Due (Some b, Some a)

(* Matching *)

let matches_context s entry =
  List.exists ~f:(String.is_substring ~substring:s) (Entry.context_tags entry)

let matches_project ps entry =
  let rec f pat project = match (pat, project) with
    | [], l -> true
    | l, [] -> false
    | h :: t, h' :: t' -> (String.is_substring ~substring:h h') && (f t t')
  in
  List.exists ~f:(f ps) (Entry.project_tags entry)

let matches_date a b entry = 
  let dates = Entry.due_tags entry in
  match (a, b) with
    | (None, None)     -> true
    | (Some a, None)   -> List.exists ~f:(Date.(<=) a) dates
    | (None, Some b)   -> List.exists ~f:(Date.(>=) b) dates
    | (Some a, Some b) -> 
        List.exists ~f:(fun d -> Date.(a <= d && d <= b)) dates

let rec matches sel ?(index=0) entry = match sel with
  | And (a, b) -> matches a ~index entry && matches b ~index entry
  | Or  (a, b) -> matches a ~index entry || matches b ~index entry
  | Not a      -> not (matches a ~index entry)
  | Index i    -> index = i
  | Text s     -> String.is_substring entry.Entry.text ~substring:s
  | Context s  -> matches_context s entry
  | Project ps -> matches_project ps entry
  | Due (a, b) -> matches_date a b entry
  | False      -> false
  | True       -> true
  | Priority p -> Entry.(prio_to_int p = prio_to_int entry.prio)

let filter sel l = List.filteri ~f:(fun index x -> matches sel ~index x) l
let split sel l = (filter sel l, filter (not' sel) l)


module P = struct
  open Parser

  (* Writing *)

  let date_range_to_string a b = String.concat ~sep:" " [
      Option.value_map ~default:"" ~f:Date.to_string a
    ; "-"
    ; Option.value_map ~default:"" ~f:Date.to_string b
  ]

  let rec to_string = function
    | Or (False, b) -> to_string b
    | Or (a, b)     -> "(" ^ to_string a ^ " or " ^ to_string b ^ ")"
    | And (a, b)    -> to_string a ^ " and " ^ to_string b
    | Not a         -> "~" ^ to_string a
    | Index i       -> Int.to_string i
    | Text pat      -> "\'" ^ pat ^ "\'"
    | Project ps    -> Tag.(to_string (Project ps))
    | Context ps    -> Tag.(to_string (Context ps))
    | Priority p    -> Entry.P.prio_to_string p
    | Due (a, b)    -> date_range_to_string a b
    | True | False -> raise ImpossibleError

  (* Reading *)

  let and_mark = string "and"
  let or_mark  = string "or"  <|> string ","
  let not_mark = string "not" <|> string "~"

  let quote_mark = char '"' <|> char '\''
  let range_mark = char '-'

  let opt_tagmarker = opt (char Tag.P.tagmarker)

  let text =
    let str = take_till (function '"' | '\'' -> true | _ -> false) in
    lift text (quote_mark *> str <* quote_mark)

  let due_range =
    let date = opt_tagmarker *> Tag.P.date_tag in
    list [opt date <* ws <* range_mark; ws *> opt date] >>= function
      | [None; None] -> fail "invalid range"
      | [a; b]       -> return (due_range a b)
      | _            -> raise ImpossibleError

  let index_range =
    let range a b =
      List.range a b
      |> List.map ~f:index
      |> List.fold ~init:False ~f:or'
    in
    lift2 range (integer <* ws <* range_mark) (ws *> integer)

  let due   = lift due (opt_tagmarker *> Tag.P.date_tag)
  let index = lift index integer

  let context = Tag.P.(opt_tagmarker *> lift context context_tag)
  let project = Tag.P.(opt_tagmarker *> lift project project_tag)
  let prio    = lift prio Entry.P.prio


  let fixpoint p =
    let and' = ws *> and_mark *> ws *> return and' in
    let or'  = ws *> or_mark *> ws *> return or' in
    let not' = lift not' (not_mark *> ws *> p) in
    let par  = char '(' *> ws *> p <* ws <* char ')' in
    let factor = choice [
        due_range
      ; due
      ; index_range
      ; index
      ; text
      ; prio
      ; project
      ; context 
      ; not'
      ; par
    ]
    in
    chainl1 (chainl1 factor and') or'

  let parser = fix fixpoint

  let of_string = parse_string (only parser)

end

let to_string = P.to_string
let of_string = P.of_string
let of_string_exn str =
  match of_string str with
    | Ok sel -> sel
    | Error err -> raise (ArgumentError err)

