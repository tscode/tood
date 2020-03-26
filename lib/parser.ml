open Types

include Parser_base

let date = Date.P.parser 
let tag  = Tag.P.parser ~marker:Tag.P.tagmarker

let entry = Entry.P.parser
let entry_strict = Entry.P.parser_strict

let filter = Filter.P.parser
let mods   = Mod.P.parser_list

