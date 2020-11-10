open Types

type pattern = String.t * Str.regexp

let pattern s = (s, Str.regexp_string s)
let matches_pattern pat str = Str.string_match (snd pat) str 0

(* index to index range! *)

type t =
  | And        of t * t
  | Or         of t * t
  | Not        of t
  | Index      of (int * int)
  | Text       of pattern
  | Project    of pattern list
  | Subproject of pattern
  | Context    of pattern
  | Priority   of Entry.priority
  | Due        of (Date.t option * Date.t option)
  | True
  | False

let and' a b  = And (a, b) 
let or'  a b  = Or (a, b)
let not' a    = Not a

let idx i  = Index (i, i)
let due a  = Due (Some a, Some a)
let text p = Text (pattern p)
let prio a = Priority a

let context p    = Context (pattern p)
let project p    = Project (List.map pattern p)
let subproject p = Subproject (pattern p)

let index_range a b = Index (a, b)

let due_range a b = match (a, b) with
  | _, None | None, _ -> Due (a, b)
  | Some a, Some b -> match Date.(a <= b) with
    | true  -> Due (Some a, Some b)
    | false -> Due (Some b, Some a)

(* Matching *)

let matches_context s entry =
  List.exists (matches_pattern s) (Entry.context_tags entry)

let matches_project ps entry =
  let rec f pat project = match (pat, project) with
    | [], _ -> true
    | _, [] -> false
    | h :: t, h' :: t' -> (matches_pattern h h') && (f t t')
  in
  List.exists (f ps) (Entry.project_tags entry)

let matches_subproject p entry =
  List.exists (List.exists (matches_pattern p)) (Entry.project_tags entry)

let matches_date a b entry = 
  let dates = Entry.due_tags entry in
  match (a, b) with
    | (None, None)     -> true
    | (Some a, None)   -> List.exists (Date.(<=) a) dates
    | (None, Some b)   -> List.exists (Date.(>=) b) dates 
    | (Some a, Some b) -> List.exists (fun d -> Date.(a <= d && d <= b)) dates

let rec matches sel ?(index=0) entry = match sel with
  | And (a, b)   -> matches a ~index entry && matches b ~index entry
  | Or  (a, b)   -> matches a ~index entry || matches b ~index entry
  | Not a        -> not (matches a ~index entry)
  | Index (i, j) -> index >= i && index <= j
  | Text pat     -> matches_pattern pat Entry.(entry.text)
  | Context pat  -> matches_context pat entry
  | Project ps   -> matches_project ps entry
  | Subproject p -> matches_subproject p entry
  | Due (a, b)   -> matches_date a b entry
  | False        -> false
  | True         -> true
  | Priority p   -> Entry.(prio_to_int p = prio_to_int entry.prio)


let filter sel l = List.filteri (fun i x -> matches sel ~index:(i+1) x) l
let split sel l = (filter sel l, filter (not' sel) l)
let map sel f l =
  let f entry = match matches sel entry with
    | false -> entry
    | true  -> f entry
  in
  List.map f l


let filter_indexed sel l = 
  let f (index, entry) = match matches sel ~index entry with
    | true  -> Some (index, entry)
    | false -> None
  in
  List.filter_map f l

let split_indexed sel l = (filter_indexed sel l, filter_indexed (not' sel) l)

let map_indexed sel f l =
  let f (index, entry) = match matches sel ~index entry with
    | false -> (index, entry)
    | true  -> (index, f entry)
  in
  List.map f l

module P = struct
  open Parser_base

  (* Writing *)

  let index_range_to_string i j = match i = j with
  | true -> Int.to_string i
  | false -> Int.to_string i ^ ".." ^ Int.to_string j

  let date_range_to_string a b = match a = b with
  | true -> Option.fold ~none:"" ~some:Date.to_string a
  | false -> String.concat " "
    [ Option.fold ~none:"" ~some:Date.to_string a
    ; ".."
    ; Option.fold ~none:"" ~some:Date.to_string b ]

  let rec to_string = function
    | Or (False, b) -> to_string b
    | Or (a, b)     -> "(" ^ to_string a ^ " or " ^ to_string b ^ ")"
    | And (a, b)    -> to_string a ^ " and " ^ to_string b
    | Not a         -> "~" ^ to_string a
    | Index (i, j)  -> index_range_to_string i j
    | Text pat      -> "'" ^ (fst pat) ^ "'"
    | Project ps    -> Tag.(List.map fst ps |> project |> to_string)
    | Subproject p  -> "/" ^ fst p ^ "/"
    | Context pat   -> Tag.(fst pat |> context |> to_string)
    | Priority p    -> Entry.P.prio_to_string p
    | Due (a, b)    -> date_range_to_string a b
    | True          -> "<true>"
    | False         -> "<false>"

  (* Reading *)

  let and_mark = string "and"
  let or_mark  = string "or"  <|> string ","
  let not_mark = string "not" <|> string "~"

  let quote_mark = char '\'' <|> char '"'
  let range_mark = string ".."

  let tagmarker = char Tag.P.tagmarker
  let opt_tagmarker = opt (char Tag.P.tagmarker) 

  let text = lift text (quoted '\'' <|> quoted '"')

  let due_range fmt =
    let date = opt_tagmarker *> Date.P.parser ~fmt () in
    (lift2 due_range (date <* ws <* range_mark >>| Option.some) (date >>| Option.some))
    <|>
    (lift (due_range None) (range_mark *> ws *> date >>| Option.some))
    <|>
    (lift (fun x -> due_range x None) (date <* ws <* range_mark >>| Option.some))

  let index_range = lift2 index_range (integer <* ws <* range_mark) (ws *> integer)

  let due fmt = lift due (opt_tagmarker *> Date.P.parser ~fmt ())
  let index = lift idx integer

  let context = tagmarker *> Tag.P.(lift context context_tag)
  let project = tagmarker *> Tag.P.(lift project project_tag)
  let subproject = char '/' *> lift subproject symbol <* char '/'

  let prio    = lift prio Entry.P.prio
  let boolean =
    string "<true>" *> return True <|> string "<false>" *> return False

  let wildcard = symbol >>| fun sym ->
    let p = pattern sym in
    or' (Text p) (or' (Context p)  (Subproject p))


  let fixpoint fmt p =
    let and' = ws *> and_mark *> ws *> return and' in
    let or'  = ws *> or_mark *> ws *> return or' in
    let par  = char '(' *> ws *> p <* ws <* char ')' in
    let factor = choice [
        due_range fmt
      ; due fmt
      ; index_range
      ; index
      ; text
      ; prio
      ; wildcard
      ; project
      ; subproject
      ; context 
      ; boolean
      ; par
    ]
    in
    let not' = lift not' (not_mark *> ws *> factor) in
    chainl1 (chainl1 (not' <|> factor) and') or'

  let parser ?(fmt=Date.default_fmt) () =
    fix (fixpoint fmt) <|> ws *> return True

  let of_string ?fmt str =
    match parse_string ~consume:All (only (parser ?fmt ())) str with
    | Ok sel -> Ok sel
    | Error _ -> Error ("cannot parse selector '" ^ str ^ "'")

end

let to_string = P.to_string
let of_string = P.of_string

let of_string_exn ?fmt str =
  match of_string ?fmt str with
    | Ok sel -> sel
    | Error err -> raise (ArgumentError err)

