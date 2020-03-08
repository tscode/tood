open Types

type t = symbol

let to_string sym = sym
let of_string = Angstrom.parse_string Parser.(only symbol)

let of_string_exn str = match of_string str with
  | Ok sym -> sym
  | Error err -> raise (ArgumentError err)
