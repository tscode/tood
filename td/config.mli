
exception ParseError of string

type t
type options

val fallback : t
val default : ?listname : string -> string -> string -> t

val to_string : t -> string
val parse_file :
  string -> [`Ok of t | `Not_found of string | `Invalid of string]

val default_path : unit -> string

val names : t -> string list
val options : t -> string -> options option
val options_exn : t -> string -> options

val get : options -> string -> string
val get_safe : options -> string -> string option

val add : options -> string -> string -> (options, string) result
val merge : options -> options -> options

val valid_option_keys : string list
val check_option :
  string -> string -> [`Ok | `KeyError of string | `ValueError of string]

