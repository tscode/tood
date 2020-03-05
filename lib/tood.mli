
type 'a res = ('a, string) Result.t


exception ParseError    of string
exception ArgumentError of string


module type S = sig
  type t

  val to_string     : t -> string
  val of_string     : string -> t res
  val of_string_exn : string -> t
end


module Symbol : S


module Date : sig
  include S

  val create     : day : int -> month : int -> year : int -> t res
  val create_exn : day : int -> month : int -> year : int -> t

  val day   : t -> int
  val month : t -> int
  val year  : t -> int

  val is_valid : int -> int -> int -> bool

  (* Date formatting *)

  type fmt

  val fmt           : string -> fmt
  val default_fmt   : fmt

  val fmt_to_string : fmt -> string
  val format        : fmt : fmt -> ?strip : bool -> t -> string

  (* Date parsing with custom formating 
   *
   * This will only work if the format string contains the placeholders %d, %m,
   * %y exactly once each, and if the char '%' does not appear otherwise.
   * Then is_fmt_legible fmt = true.
   *
   * *)

  val is_fmt_legible   : fmt -> bool

  val of_string_fmt     : fmt : fmt -> string -> t res
  val of_string_fmt_exn : fmt : fmt -> string -> t
end


module Tag : sig
  type t =
    | Context of Symbol.t
    | Project of Symbol.t list
    | Due     of Date.t

  include S with type t := t

  val context : Symbol.t -> t
  val project : Symbol.t list -> t
  val due     : Date.t -> t
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

  val index      : t list -> (int * t) list
  val index_with : int list -> t list -> (int * t) list res

  val min_index : (int * t) list -> int option
  val max_index : (int * t) list -> int option

  val deindex : (int * t) list -> t list
  val indices : (int * t) list -> int list

  (* Less strict parsing with custom date formats *)

  val of_string_relaxed     : ?fmt_date : Date.fmt -> string -> t res
  val of_string_relaxed_exn : ?fmt_date : Date.fmt -> string -> t

  (* Entry formatting *)

  type fmt

  val fmt : string -> fmt
  val default_fmt : fmt

  val fmt_to_string : fmt -> string

  val format : fmt       : fmt      ->
               ?fmt_date : Date.fmt ->
               ?strip    : bool     ->
               ?tag_sep  : string   -> t -> string

  val format_index : fmt       : fmt      -> 
                     fmt_date  : Date.fmt -> 
                     max_index : int      -> 
                     ?tag_sep  : string   -> int * t -> string
end

module Select : sig
  type t

  val of_string     : ?fmt_date : Date.fmt -> string -> t res
  val of_string_exn : ?fmt_date : Date.fmt -> string -> t
  val to_string : t -> string

  val and' : t -> t -> t
  val or'  : t -> t -> t
  val not' : t -> t

  val idx  : int -> t

  val text    : string -> t
  val project : string list -> t
  val context : string -> t
  val prio    : Entry.priority -> t

  val due : Date.t -> t
  val due_range : Date.t option -> Date.t option -> t

  (** [matches ?index sel entry] indicates if [sel] matches [entry] with index
   ** [index]. If no [index] is provided each positive index selector will
   ** evalute to false. **)
  val matches : t -> ?index : int -> Entry.t -> bool
  val filter  : t -> Entry.t list -> Entry.t list
  val split   : t -> Entry.t list -> Entry.t list * Entry.t list

  val filter_indexed : t -> (int * Entry.t) list -> (int * Entry.t) list
  val split_indexed  : t -> (int * Entry.t) list 
                         -> (int * Entry.t) list * (int * Entry.t) list
end

module Parser = Parser

