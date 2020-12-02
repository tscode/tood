open Tood

let is_color c = 1 <= c && c <= 255

let color256 str =
  let r, str = match String.(length str > 3 && sub str 0 3 = "on-") with
  | false -> 38, str
  | true -> 48, String.(sub str 3 (length str - 3))
  in
  match int_of_string_opt str with
  | Some c when is_color c -> Printf.sprintf "%d;5;%d" r c
  | _ -> raise (ArgumentError str)

let parse_code = function
  | "bold"       -> "1"
  | "dim"        -> "2"
  | "underlined" -> "4"
  | "blink"      -> "5"

  | "white"   -> "97" | "on-white"   -> "107"
  | "black"   -> "30" | "on-black"   -> "40"
  | "red"     -> "31" | "on-red"     -> "41"
  | "green"   -> "32" | "on-green"   -> "42"
  | "yellow"  -> "33" | "on-yellow"  -> "43"
  | "blue"    -> "34" | "on-blue"    -> "44"
  | "magenta" -> "35" | "on-magenta" -> "45"
  | "cyan"    -> "36" | "on-cyan"    -> "46"
  | "gray"    -> "90" | "on-gray"    -> "100"
  | "grey"    -> "90" | "on-grey"    -> "100"

  | "light-red"     -> "91" | "on-light-red"     -> "101" 
  | "light-green"   -> "92" | "on-light-green"   -> "102"
  | "light-yellow"  -> "93" | "on-light-yellow"  -> "103"
  | "light-blue"    -> "94" | "on-light-blue"    -> "104"
  | "light-magenta" -> "95" | "on-light-magenta" -> "105"
  | "light-cyan"    -> "96" | "on-light-cyan"    -> "106"
  | "light-gray"    -> "37" | "on-light-gray"    -> "47"
  | "light-grey"    -> "37" | "on-light-grey"    -> "47"

  | str -> color256 str

let apply codes =
  Printf.sprintf "\x1B[%sm%s\x1B[0m" (String.concat ";" codes)

