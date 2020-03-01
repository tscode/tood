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

let d = create "d" "Day in a month (int)"
let m = create "m" "Month in a year (int)"
let y = create "y" "Year (int)"

let i  = create "i" "Index of an entry (int)"
let i' = create "I" "Index of an entry with right padding (int)"
let j' = create "J" "Index of an entry with left padding (int)"

let r  = create "r" "Entry priority (?, -, !)"
let t  = create "t" "Entry text (string)"
let t' = create "T" "Entry tags (string list)"
let p  = create "p" "Project tags without tagmarks (string list)"
let p' = create "P" "Project tags with tagmarks (string list)"
let c  = create "c" "Context tags without tagmarks (string list)"
let c' = create "C" "Context tags with tagmarks (string list)"
let d  = create "d" "Date tags without tagmarks (string list)"
let d' = create "D" "Date tags with tagmarks (string list)"

let maybe_sep sep = function
  | "" -> ""
  | a  -> a ^ sep

let sub ~sep (s, value) content =
  let with_ = maybe_sep sep value in
  let content = SP.replace_all s.pat_ ~in_:content ~with_ in
  SP.replace_all s.pat ~in_:content ~with_:value

let date_substitutions = [
  d; m; y
]

let entry_substitutions = [
  i; i'; j'; r; t; t'; p; p'; c; c'; d; d'
]

let substitutions =
  date_substitutions @ entry_substitutions

