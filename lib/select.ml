open Types

type pat = String.t * String.Search_pattern.t

let pat s = (s, String.Search_pattern.create s)
let matches_pat pat str = String.Search_pattern.matches (snd pat) str

type t =
  | And        of t * t
  | Or         of t * t
  | Not        of t
  | Index      of int
  | Text       of pat
  | Project    of pat list
  | Subproject of pat
  | Context    of pat
  | Priority   of Entry.priority
  | Due        of (Date.t option * Date.t option)
  | True
  | False

let and' a b  = And (a, b) 
let or'  a b  = Or (a, b)
let not' a    = Not a

let idx i  = Index i
let due a  = Due (Some a, Some a)
let text p = Text (pat p)
let prio a = Priority a

let context p    = Context (pat p)
let project p    = Project (List.map ~f:pat p)
let subproject p = Subproject (pat p)

let due_range a b = match (a, b) with
  | _, None | None, _ -> Due (a, b)
  | Some a, Some b -> match Date.(a <= b) with
    | true  -> Due (Some a, Some b)
    | false -> Due (Some b, Some a)

(* Matching *)

let matches_context s entry =
  List.exists ~f:(matches_pat s) (Entry.context_tags entry)

let matches_project ps entry =
  let rec f pat project = match (pat, project) with
    | [], l -> true
    | l, [] -> false
    | h :: t, h' :: t' -> (matches_pat h h') && (f t t')
  in
  List.exists ~f:(f ps) (Entry.project_tags entry)

let matches_subproject p entry =
  List.exists ~f:(List.exists ~f:(matches_pat p)) (Entry.project_tags entry)

let matches_date a b entry = 
  let dates = Entry.due_tags entry in
  match (a, b) with
    | (None, None)     -> true
    | (Some a, None)   -> List.exists ~f:(Date.(<=) a) dates
    | (None, Some b)   -> List.exists ~f:(Date.(>=) b) dates 
    | (Some a, Some b) -> List.exists ~f:(fun d -> Date.(a <= d && d <= b)) dates

let rec matches sel ?(index=0) entry = match sel with
  | And (a, b)   -> matches a ~index entry && matches b ~index entry
  | Or  (a, b)   -> matches a ~index entry || matches b ~index entry
  | Not a        -> not (matches a ~index entry)
  | Index i      -> index = i
  | Text pat     -> matches_pat pat Entry.(entry.text)
  | Context pat  -> matches_context pat entry
  | Project ps   -> matches_project ps entry
  | Subproject p -> matches_subproject p entry
  | Due (a, b)   -> matches_date a b entry
  | False        -> false
  | True         -> true
  | Priority p   -> Entry.(prio_to_int p = prio_to_int entry.prio)

let filter sel l = List.filteri ~f:(fun i x -> matches sel ~index:(i+1) x) l
let split sel l = (filter sel l, filter (not' sel) l)

let filter_indexed sel l = 
  let f (index, entry) = match matches sel ~index entry with
    | true  -> Some (index, entry)
    | false -> None
  in
  List.filter_map ~f l

let split_indexed sel l = (filter_indexed sel l, filter_indexed (not' sel) l)

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
    | Text pat      -> "'" ^ (fst pat) ^ "'"
    | Project ps    -> Tag.(List.map ~f:fst ps |> project |> to_string)
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

  let text =
    let str = take_till (function '\'' | '"' -> true | _ -> false) in
    lift text (quote_mark *> str <* quote_mark)

  let due_range date_parser =
    let date = opt_tagmarker *> date_parser <|> Date.P.parser in
    (lift2 due_range (date <* ws <* range_mark >>| Option.some) (date >>| Option.some))
    <|>
    (lift (due_range None) (range_mark *> ws *> date >>| Option.some))
    <|>
    (lift (fun x -> due_range x None) (date <* ws <* range_mark >>| Option.some))

  let index_range =
    let range a b =
      List.range a (b+1)
      |> List.map ~f:idx
      |> List.fold ~init:False ~f:or'
    in
    lift2 range (integer <* ws <* range_mark) (ws *> integer)

  let due date_parser = lift due (opt_tagmarker *> date_parser <|> Date.P.parser)
  let index = lift idx integer

  let context = tagmarker *> Tag.P.(lift context context_tag)
  let project = tagmarker *> Tag.P.(lift project project_tag)
  let subproject = char '/' *> Tag.P.(lift subproject symbol) <* char '/'

  let prio    = lift prio Entry.P.prio
  let boolean =
    string "<true>" *> return True <|> string "<false>" *> return False

  let wildcard = symbol >>| fun sym ->
    let p = pat sym in
      or' (Text p) (or' (Context p)  (Subproject p))


  let fixpoint date_parser p =
    let and' = ws *> and_mark *> ws *> return and' in
    let or'  = ws *> or_mark *> ws *> return or' in
    let par  = char '(' *> ws *> p <* ws <* char ')' in
    let factor = choice [
        due_range date_parser
      ; due date_parser
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

  let parser date_parser =
    fix (fixpoint date_parser) <|> ws *> return True

  let of_string ?(fmt_date=Date.default_fmt) str =
    match snd fmt_date with
    | None -> Error ("date format '" ^ fst fmt_date ^ "' is invalid for parsing")
    | Some date_parser -> match parse_string (only (parser date_parser)) str with
      | Ok sel -> Ok sel
      | Error _ -> Error ("cannot parse selector '" ^ str ^ "'")

end

let to_string = P.to_string
let of_string = P.of_string

let of_string_exn ?fmt_date str =
  match of_string ?fmt_date str with
    | Ok sel -> sel
    | Error err -> raise (ArgumentError err)

