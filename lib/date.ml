open Types

type t = {
    day   : int
  ; month : int
  ; year  : int
}

let is_valid day month year = 
  let feb = if year % 4 = 0 then 29 else 28 in
  let lower = 1 <= day in
  let upper = match month with
    | 1 | 3 | 5
    | 7 | 8 | 10 | 12 -> day <= 31
    | 4 | 6 | 9  | 11 -> day <= 30
    | 2               -> day <= feb
    | _               -> false
  in
  lower && upper

let day date   = date.day
let month date = date.month
let year date  = date.year

let compare a b = Int.compare 
  (a.year * 12 * 31 + a.month * 31 + a.day)
  (b.year * 12 * 31 + b.month * 31 + b.day)

let (=) a b  = compare a b = 0
let (<=) a b = compare a b < 1
let (>=) a b = compare a b > -1
let (<) a b  = compare a b < 0
let (>) a b  = compare a b > 0

module P = struct
  open Parser

  (* Syntax *)

  let fmt = "%d.%m.%y"

  (* Writing *)

  let fill_placeholder pat value str =
    let open String.Search_pattern in
    replace_all (create pat) ~in_:str ~with_:(Int.to_string value)

  let format fmt date = fmt 
    |> fill_placeholder "%d" date.day
    |> fill_placeholder "%m" date.month
    |> fill_placeholder "%y" date.year

  let to_string date = format fmt date

  let err_msg day month year =
    "date " ^ to_string {day; month; year} ^ " invalid"

  (* Reading *)

  let create day month year = match is_valid day month year with
    | true  -> Ok {day; month; year}
    | false -> Error (err_msg day month year)

  let parser = lift3 create
  (integer <* char '.')
  (integer <* char '.')
  integer >>= function
    | Ok date   -> return date
    | Error msg -> fail msg


  let of_string = parse_string parser

end

let to_string = P.to_string
let of_string = P.of_string

let of_string_exn str =
  match of_string str with
    | Ok date   -> date
    | Error msg -> raise (ArgumentError msg)

let create = P.create

let create_exn day month year = match create day month year with
  | Ok date   -> date
  | Error msg -> raise (ArgumentError msg)

