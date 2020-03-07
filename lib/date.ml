open Types

type t = {
    day   : int
  ; month : int
  ; year  : int
}

type fmt = string * t Parser.t option

let is_fmt_legible = function
  | (_, Some _) -> true
  | (_, None)   -> false

let is_valid ~day ~month ~year = 
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

let day   date = date.day
let month date = date.month
let year  date = date.year

let compare a b = Int.compare 
  (a.year * 12 * 31 + a.month * 31 + a.day)
  (b.year * 12 * 31 + b.month * 31 + b.day)

let (=)  a b = compare a b = 0
let (<=) a b = compare a b < 1
let (>=) a b = compare a b > -1
let (<)  a b = compare a b < 0
let (>)  a b = compare a b > 0

(* Module for parsing / writing related functions *)

module P = struct
  open Parser

  (* Default date format string *)

  let default_fmt_string     = "%y-%m-%d" (* ISO 8601 *)
  let default_fmt_unresolved = ("%y-%m-%d", Error "not initialized yet")

  (* Writing *)

  let format ~fmt ?(strip=true) date =
    let open Sub in
    let output = (fst fmt)
    |> sub ~sep:"" (d, date.day   |> Int.to_string)
    |> sub ~sep:"" (m, date.month |> Int.to_string)
    |> sub ~sep:"" (y, date.year  |> Int.to_string)
    in
    if strip then String.strip output else output

  let to_string date = format ~fmt:default_fmt_unresolved date

  (* Reading default date format *)

  let err_msg day month year =
    "date '" ^ to_string {day; month; year} ^ "' is invalid"

  let create year month day = match is_valid year month day with
    | true  -> Ok {day; month; year}
    | false -> Error (err_msg day month year)

  let parser = lift3 create
  (integer <* char '-')
  (integer <* char '-')
  integer >>= function
    | Ok date   -> return date
    | Error msg -> fail msg

  (* Reading custom date formats *)

  (* The following two auxiliary functions relies on the fact that `Day < `Month
   * < `Year for the polymorphic compare functions in module Poly *)
  let date_of_polyvar_list fmt_str l =
    match Poly.(List.sort ~compare l) with
    | [`Day d; `Month m; `Year y] -> create y m d
    | _ -> match l with
      | [`Fail str] -> Error
        ("string '" ^ str ^ "' does not match date format '" ^ fmt_str ^ "'")
      | _ -> raise ImpossibleError

  let has_day_month_year p1 p2 p3 =
    Poly.(List.sort ~compare [p1; p2; p3;] = [`Day; `Month; `Year])

  let fmt_invalid_parsing_msg fmt_str =
    "date format '" ^ (fmt_str) ^ "' is invalid for parsing"

  let parser_fmt fmt_str =
    let str = take_till_unescaped (function '%' -> true | _ -> false) in
    let sub = choice [
        string "%d" *> return `Day
      ; string "%m" *> return `Month
      ; string "%y" *> return `Year
    ] in
    let date_fail str = [`Fail str] in
    let date_pair var value = match var with
      | `Day   -> `Day   value
      | `Month -> `Month value
      | `Year  -> `Year  value
    in
    let f t1 a t2 b t3 c t4 =
      match has_day_month_year a b c with
      | false -> Error (fmt_invalid_parsing_msg fmt_str)
      | true  -> Ok begin
        let date_parser = list [ 
            (string t1 *> integer >>| date_pair a)
          ; (string t2 *> integer >>| date_pair b)
          ; (string t3 *> integer >>| date_pair c) <* string t4 
        ] <|> (take_all           >>| date_fail)
        in
        date_parser >>| date_of_polyvar_list fmt_str >>= function
        | Ok date   -> return date
        | Error err -> fail err
      end
    in
    let fmt_parser = 
      lift4 f str sub str sub <*> str <*> sub <*> str >>= function
      | Ok date_parser -> return date_parser
      | Error err      -> fail err
    in
    parse_string (only fmt_parser) fmt_str

  let fmt fmt_str = match parser_fmt fmt_str with
    | Ok date_parser -> (fmt_str, Some date_parser)
    | Error _        -> (fmt_str, None)

  let default_fmt = fmt default_fmt_string

  let of_string ?(fmt=default_fmt) str = match snd fmt with
    | Some p -> parse_string (only p) str
    | None   -> Error (fmt_invalid_parsing_msg (fst fmt))


end

(* Bring parsing related functions in main Entry module *)

let fmt = P.fmt
let fmt_to_string = fst
let default_fmt = P.default_fmt

let with_fmt ?(fmt=default_fmt) ~f = match snd fmt with
  | Some parser -> f (fst fmt) parser
  | None        -> Error (P.fmt_invalid_parsing_msg (fst fmt))

let format = P.format
let to_string = P.to_string
let of_string = P.of_string


let of_string_exn ?fmt str =
  match of_string ?fmt str with
    | Ok date   -> date
    | Error msg -> raise (ArgumentError msg)

let create ~day ~month ~year = P.create year month day

let create_exn ~day ~month ~year = match create ~day ~month ~year with
  | Ok date   -> date
  | Error msg -> raise (ArgumentError msg)

