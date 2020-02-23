include Types

module type S = sig
  type t

  val to_string : t -> string
  val of_string : string -> t res
  val of_string_exn : string -> t
end

module Symbol = struct

  type t = symbol

  let to_string sym = sym
  let of_string = Angstrom.parse_string Parser.(only symbol)

  let of_string_exn str = match of_string str with
    | Ok sym -> sym
    | Error err -> raise (ArgumentError err)

end

module Parser = Parser
module Date   = Date
module Tag    = Tag
module Entry  = Entry
module Select = Select

