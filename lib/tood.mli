
type 'a res    = ('a, string) Result.t

module Symbol : sig

  type t

  val to_string     : t -> string
  val of_string     : string -> t res
  val of_string_exn : string -> t

end

module Date : sig

  type t

  val create     : day : int -> month : int -> year : int -> t res
  val create_exn : day : int -> month : int -> year : int -> t

  val day   : t -> int
  val month : t -> int
  val year  : t -> int

  val is_valid : day : int -> month : int -> year : int -> bool

  (* Custom parsable date formats *)

  type fmt

  val fmt     : string -> fmt res
  val fmt_exn : string -> fmt

  val default_fmt        : fmt
  val fmt_to_string      : fmt -> string
  val default_fmt_string : string

  val to_string     : ?fmt : fmt -> t -> string
  val of_string     : ?fmt : fmt -> string -> t res
  val of_string_exn : ?fmt : fmt -> string -> t

  (* String to date conversion with arbitrary format strings *)

  val format : string -> t -> string

end

module Tag : sig

  type t =
    | Context of Symbol.t
    | Project of Symbol.t list
    | Due     of Date.t

  val to_string     : ?fmt : Date.fmt -> t -> string
  val of_string     : ?fmt : Date.fmt -> string -> t res
  val of_string_exn : ?fmt : Date.fmt -> string -> t

  val context : Symbol.t -> t
  val project : Symbol.t list -> t
  val due     : Date.t -> t

end

module Entry : sig

  type t

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

  (* Indexed entries *)

  val index      : t list -> (int * t) list
  val index_with : int list -> t list -> (int * t) list res

  val min_index : (int * t) list -> int option
  val max_index : (int * t) list -> int option

  val deindex : (int * t) list -> t list
  val indices : (int * t) list -> int list

  (* Writing of entries *)
  val to_string        : ?fmt : Date.fmt -> t -> string
  val to_string_strict : t -> string

  (* Less strict parsing with custom date formats *)

  val of_string     : ?fmt : Date.fmt -> string -> t res
  val of_string_exn : ?fmt : Date.fmt -> string -> t

  (* Strict printing / parsing that does not tolerate deviations from the
   * 'official' format *)

  val of_string_strict     : string -> t res
  val of_string_strict_exn : string -> t

  (* Entry formatting *)

  type part =
    | Index
    | Prio
    | Text
    | Tag
    | Project
    | Context
    | Date

  val default_layout : string

  val format :
    ?style : (t -> part -> string -> string) ->
    ?fmt   : Date.fmt ->
    ?sep   : string -> string -> t -> string

  val format_indexed :
    ?style     : (t -> part -> string -> string) ->
    ?fmt       : Date.fmt -> 
    ?max_index : int -> 
    ?sep       : string -> string -> int * t -> string

end

module Filter : sig

  type t

  val of_string     : ?fmt : Date.fmt -> string -> t res
  val of_string_exn : ?fmt : Date.fmt -> string -> t
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
  val map     : t -> (Entry.t -> Entry.t) -> Entry.t list -> Entry.t list

  val filter_indexed : t -> (int * Entry.t) list -> (int * Entry.t) list

  val split_indexed  :
    t                    -> 
    (int * Entry.t) list ->
    (int * Entry.t) list * (int * Entry.t) list

  val map_indexed :
    t                    ->
    (Entry.t -> Entry.t) ->
    (int * Entry.t) list ->
    (int * Entry.t) list

end

module Mod : sig

  type t

  val text : string -> t
  val prio : Entry.priority -> t

  val add_tag : Tag.t -> t
  val del_tag : Tag.t -> t

  val is_text : t -> bool
  val is_prio : t -> bool
  val is_add_tag : t -> bool
  val is_del_tag : t -> bool

  val apply      : t -> Entry.t -> Entry.t
  val apply_list : t list -> Entry.t -> Entry.t

  val to_string     : ?fmt : Date.fmt -> t -> string
  val of_string     : ?fmt : Date.fmt -> string -> t res
  val of_string_exn : ?fmt : Date.fmt -> string -> t

  val list_of_string     : ?fmt : Date.fmt -> string -> t list res
  val list_of_string_exn : ?fmt : Date.fmt -> string -> t list

end

module Tree : sig

  type 'a t
  type path = Symbol.t list

  val name   : 'a t -> Symbol.t
  val leaves : 'a t -> 'a list
  val nodes  : 'a t -> 'a t list

  val create : ?leaves : 'a list -> ?nodes : 'a t list -> Symbol.t -> 'a t

  val has_name : Symbol.t -> 'a t -> bool

  val add_leaf  : ?path : path -> 'a t -> 'a -> 'a t
  val of_leaves : ?name : Symbol.t -> paths : ('a -> path list) -> 'a list -> 'a t
  val to_leaves : 'a t -> 'a list

  val map : ('a -> 'b) -> 'a t -> 'b t

  val collect :
    ?path  : path ->
    ?fname : (path -> Symbol.t -> 'b) ->
    (path -> 'a -> 'b) -> 'a t -> 'b list

end

module Parser : sig

  type 'a t = 'a Angstrom.t

  val is_symbol_start : char -> bool
  val is_symbol_char : char -> bool

  val ws   : unit t
  val eoil : unit t

  val opt : 'a t -> 'a option t

  val integer : int t
  val quoted  : char -> string t

  val take_all : string t
  val take_till_unescaped : (char -> bool) -> string t
  val take_till_unescaped_char : char -> string t
  val only : 'a t -> 'a t

  val symbol : Symbol.t t

  val date   : ?fmt : Date.fmt -> unit -> Date.t t
  val tag    : ?fmt : Date.fmt -> unit -> Tag.t t
  val entry  : ?fmt : Date.fmt -> unit -> Entry.t t
  val filter : ?fmt : Date.fmt -> unit -> Filter.t t
  val mods   : ?fmt : Date.fmt -> unit -> Mod.t list t

  val entry_strict : Entry.t t 

end

(* Substitution of placeholders *)

(*
module Sub : sig

  type t

  val symbol : t -> Symbol.t
  val info : t -> string

  val date_patterns : t list
  val entry_patterns : t list
  val patterns : t list

end
*)

(* Exceptions *)

exception ParseError    of string
exception ArgumentError of string

