
type 'a res = ('a, string) Result.t

exception ParseError    of string
exception ArgumentError of string


module type S = sig
  type t

  val to_string : t -> string
  val of_string : string -> t res
  val of_string_exn : string -> t
end

module Symbol : S

module Date : sig
  include S

  val is_valid : int -> int -> int -> bool
end

module Tag : sig
  type t =
    | Context of Symbol.t
    | Project of Symbol.t list
    | Due     of Date.t

  include S with type t := t

  val context : Symbol.t -> t
  val project : Symbol.t list -> t
  val due : Date.t -> t
end

module Entry : sig
  include S

  type priority =
    | High
    | Default
    | Low

  val create : ?tags : Tag.t list -> ?prio : priority -> string -> t

  val text : t -> string
  val prio : t -> priority
  val tags : t -> Tag.t list

  val context_tags : t -> Symbol.t list
  val project_tags : t -> Symbol.t list list
  val due_tags     : t -> Date.t list

  val of_string_relaxed : string -> t res
  val of_string_relaxed_exn : string -> t
end

module Select : sig
  include S

  type pat = Symbol.t

  val and' : t -> t -> t
  val or'  : t -> t -> t
  val not' : t -> t

  val index : int -> t

  val text    : string -> t
  val project : pat list -> t
  val context : pat -> t
  val prio    : Entry.priority -> t

  val due : Date.t -> t
  val due_range : Date.t option -> Date.t option -> t

  (** [matches ?index sel entry] indicates if [sel] matches [entry] with index
   ** [index]. If no [index] is provided each positive index selector will
   ** evalute to false. **)
  val matches : t -> ?index : int -> Entry.t -> bool
  val filter  : t -> Entry.t list -> Entry.t list
  val split   : t -> Entry.t list -> Entry.t list * Entry.t list

end

