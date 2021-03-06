type 'a res = ('a, string) Result.t

type symbol = string

type priority =
  | High
  | Default
  | Low

type 'a parser = 'a Angstrom.t

exception ImpossibleError
exception ParseError of string
exception ArgumentError of string

