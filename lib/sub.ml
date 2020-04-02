open Types

module SP = String.Search_pattern

type t = {
    symbol : string
  ; info   : string
  ; pat    : SP.t
  ; pat_   : SP.t
}

let prefix = "%"

let create symbol info = {
    symbol
  ; info
  ; pat  = SP.create (prefix ^ symbol)
  ; pat_ = SP.create (prefix ^ symbol ^ "_")
}

let symbol a = a.symbol
let info a   = a.info

let d = create "d" "day in a month (int)"
let m = create "m" "month in a year (int)"
let y = create "y" "year (int)"

let i  = create "i" "index of an entry (int)"
let i' = create "I" "index of an entry with right padding (int)"
let j' = create "J" "index of an entry with left padding (int)"

let r  = create "r" "entry priority (?, -, !)"
let t  = create "t" "entry text (string)"
let t' = create "T" "entry tags (string list)"
let p  = create "p" "project tags without tagmarks (string list)"
let p' = create "P" "project tags with tagmarks (string list)"
let c  = create "c" "context tags without tagmarks (string list)"
let c' = create "C" "context tags with tagmarks (string list)"
let d  = create "d" "date tags without tagmarks (string list)"
let d' = create "D" "date tags with tagmarks (string list)"

let maybe_sep sep = function
  | "" -> ""
  | a  -> a ^ sep

let sub ~sep (s, value) content =
  let with_ = maybe_sep sep value in
  let content = SP.replace_all s.pat_ ~in_:content ~with_ in
  SP.replace_all s.pat ~in_:content ~with_:value

let date_patterns = [
  d; m; y
]

let entry_patterns = [
  i; i'; j'; r; t; t'; p; p'; c; c'; d; d'
]

let patterns = date_patterns @ entry_patterns

