
open Cmdliner

let main = [
  `S Manpage.s_description;
  `P "Td is a command line tool crafted to manipulate and display todo
  lists. It contains commands to create, modify, remove, and selectively print
  todo list entries, working with human readable files in the simple $(b,tood)
  format (named after the ocaml library 'tood' that powers td). 
  The tood format supports context, project, and date tags, which can be
  attached to entries at will, and which allow for flexible filtering and
  sorting operations.";
  `P "To get started, run $(b,td init). This will generate a configuration
  file with default options at $(b,~/.config/td/config) (or at the path pointed
  to by the environment variable TD_CONFIG) and generate two empty tood files
  that are used to store active (unfinished) and done (finished) todo entries.
  The former is called todo-file and the latter is called done-file throughout
  this documentation. Alternatively, you can create a configuration file by hand
  and run $(b,td init) to touch the respective tood files.";
  `P "Td works by modifying todo-file and done-file with the various commands
  listed below. Documentation of the configuration options and other
  features, like filtering, formatting, and tagging, can be displayed by running
  $(b,td init --help).";
]


let init = 
  let entry_placeholders =
    [ `P "$(b,%i) : index of the entry"
    ; `P "$(b,%I) : index of the entry with right padding"
    ; `P "$(b,%J) : index of the entry with left padding"
    ; `P "$(b,%r) : entry priority (?, -, !)"
    ; `P "$(b,%s) : entry text"
    ; `P "$(b,%t) : entry tags without tagmarks"
    ; `P "$(b,%T) : entry tags with tagmarks"
    ; `P "$(b,%p) : project tags without tagmarks"
    ; `P "$(b,%P) : project tags with tagmarks"
    ; `P "$(b,%c) : context tags without tagmarks"
    ; `P "$(b,%C) : context tags with tagmarks"
    ; `P "$(b,%d) : date tags without tagmarks"
    ; `P "$(b,%D) : date tags with tagmarks" ]
  in
  let date_placeholders =
    [ `P "$(b,%y) : year"
    ; `P "$(b,%m) : month in the current year"
    ; `P "$(b,%d) : day in the current month" ]
  in
  let description = [
  `S Manpage.s_description;
  `P "Initializes the td configuration file with default options (if it does not
  already exist at the config path) and touches the specified todo and done
  files in order to make sure that they can be accessed. The configuration
  path is '~/config/td/config' by default, but can be overwritten by the
  environment variable $(b,TD_CONFIG) or by providing the command line argument
  $(b,--config).";
  `P "On invokation, general information about the usage of td is
  printed unless the flag $(b,--quiet) is provided."
  ]
  in
  let configuration = [
  `S "CONFIGURATION";
  `P "The td configuration file defaults to $(b,~/.config/td/config) unless
  the environment variable TOOD_CONFIG is defined. It can also be specified
  explicitly by the $(b,--config) option for all subcommands of td.";
  `P "The syntax for lines in the configuration file is $(b,KEY : VALUE).
  The following keys can be specified:";
  `P "- todo_path : file where all active todo entries are stored";
  `Noblank; `P "- $(b,done-path) : file where all completed entries are stored";
  `Noblank; `P "- $(b,entry-fmt) : format of entries when printing / listing";
  `Noblank; `P "- $(b,entry-fmt-tree) : entry format when printing in tree format";
  `Noblank; `P "- $(b,date-fmt) : date format that is used for printing and parsing";
  `Noblank; `P "- $(b,sync-cmd) : shell command called when executing $(b,td sync)";
  `P "Format variables that can be used in the format strings are documented in
  the section FORMATTING."
  ]
  in
  let entries = [
  `S "ENTRIES";
  `P "Todo entries in the tood format consist of three parts: the actual text, a
  priority and a list of tags. Its textual representation is";
  `P "PRIORITY TEXT TAG1 TAG2...";
  `P "The priority can be low, middle, or high and is represented by the
  characters '?', '-', and '!'. The text representation of tags always starts
  with a '+'. Tags come in three flavors: context tags (like '+work'), project
  tags that contain at least one '/' (like '+project/' or
  '+project/subproject'), and date tags (like '+2020-05-12'). Note that td
  always uses the date format '%y-%m-%d' for storage, while custom date
  formats for user interaction can be specified in the config file.
  Examples for valid tood entries are:";
  `P "- this entry is moderately important +example +tood/documentation";
  `Noblank; `P "! this entry is important +and/has/many/subprojects";
  `P "Entries can be equipped with an arbitrary number fo tags. Due to the
  significance of the character '+' for tags, the text of entries can currently
  not contain this symbol."
  ]
  in
  let filters = [
  `S "FILTERS";
  `P "Td can filter and select tood entries with a flexible boolean filtering
  language. Basic filters are '+TAG', '\"text\"' or the symbols '?', '-', and
  '!' for exact matching of tags, text, and priorities. Indices like '5' can
  also be used to filter the entries based on their position in a tood file.
  Further supported filtering patterns are:";
  `P "- 'a..b' matches entries with index / date between a and b (inclusive)";
  `Noblank; `P "- 'and' and 'or' combine two filters logically";
  `Noblank; `P "- 'not' negates a filter";
  `Noblank; `P "- '(...)' groups filters";
  `Noblank; `P "- '/key/' searches for 'key' as substring in (sub)projects";
  `Noblank; `P "- plain words (except 'not', 'and', 'or') are matched in text and tags";
  `P "Some examples of valid filters are:";
  `P "- '+work and (not +project/)' # the parenthesis here is optional";
  `Noblank; `P "- '1..7 or 9..15' # index must be between 1 and 7 or 9 and 15";
  `Noblank; `P "- 'meet and ..2021-5-18' # entries containing 'meet' before 2021-5-18"
  ]
  in
  let formatting = [
  `S "FORMATTING";
  `P "The following placeholders can be used when specifying format entries in
  the config options $(b,entry-fmt) and $(b,entry-fmt-tree). Adding an
  underscore '_' after a pattern adds a conditional space, which will only be
  inserted if the pattern is not replaced by the empty string '', which can
  happen in case of empty lists of tags."
  ] @ entry_placeholders @ [
  `P "The following placeholders are used when specifying a date format for the
  config option $(b,date-fmt). Date formats have to parsable and are thus only
  valid if they contain each of the listed placeholders exactly once."
  ] @ date_placeholders
  in List.concat
  [ description
  ; configuration
  ; entries
  ; filters
  ; formatting ]

let add = [
  `S Manpage.s_description;
  `P "Parses the provided command line arguments as tood entry and adds it to
  the todo-file specified in the configuration. The entry has the format";
  `P "[-!?] text +tag1 +tag2 ...";
  `P "where the priority at the beginning is optional (it defaults to medium
  priority '-'). For documentation on the syntax of tags, see $(b,td init
  --help) and the examples below.";
  `S Manpage.s_examples;
  `P "The following examples are valid ways to add entries to the todo file:";
  `P "td add first entry";
  `Noblank; `P "td add ! a very important second entry +example +project/subproject";
  `Noblank; `P "td add an entry with date tag +2055-07-28";
  `P "Note that the format for the input of date tags depends on the config option
  date-fmt. So if 'date-fmt : %d.%m.%y' was set in the config file, the date tag
  above would have to be '+28.07.2055'. The format options for entries, on the
  other hand, are only used for listing / printing."
]

let mod_ = [
  `S Manpage.s_description;
  `P "Modify an entry or a selection of entries. It is possible to change
  the priority or the text of an entry, as well as to add or remove tags.
  If more than one entry is modified, a confirmation prompt is displayed (see
  -n, --noprompt). The syntax for the arguments of $(b,td mod) is";
  `P "filter mod1 [mod2 mod3...]";
  `P "where each modification can have the form (with | denoting choices)";
  `P "? | - | ! | \"TEXT\" | +TAG | ~TAG";
  `P "Note that replacement text has to be quoted with '\"' and removing tags
  requires prepending '~' instead of '+' to the tag. The syntax for filters and
  tags is documented at $(b,td init --help).";
  `S Manpage.s_examples;
  `P "The following command renames the tag '+toberemoved' to '+tobeadded'
  for all entries that have the former tag and that have an index smaller or
  equal to 55.";
  `P "td mod +toberemoved and ..55 ~toberemoved +tobeadded";
  `P "This command changes the priority of entry 7 to high and adds the
  tag '+tobeadded'.";
  `P "td mod 7 ! +tobeadded";
]

let do_ = [
  `S Manpage.s_description;
  `P "Parses the positional command line arguments as filter and moves the
  selected entries from the configured todo-file to done-file. A confirmation
  prompt is displayed if more than one entry is selected (see -n, --noprompt).
  The syntax for filters is documented at $(b,td init --help)."
]

let undo = [
  `S Manpage.s_description;
  `P "Parses the positional command line arguments as filter and moves the
  selected entries from the configured done-file to todo-file. A confirmation
  prompt is displayed if more than one entry is selected (see -n, --noprompt).
  The syntax for filters is documented at $(b,td init --help)."
]

let rm = [
  `S Manpage.s_description;
  `P "Parses the positional command line arguments as filter and removes the
  selected entries from the configured todo-file or done-file. A confirmation
  prompt is displayed if more than one entry is selected (see -n, --noprompt).
  The syntax for filters is documented at $(b,td init --help)."
]

let ls = [
  `S Manpage.s_description;
  `P "List selected entries from todo-file or done-file with custom formatting
  and style. The configuration options entry-fmt and entry-fmt-tree (if the flag
  --tree is provided) determine how the entry is formatted."
]

let sync = [
  `S Manpage.s_description;
  `P "Executes the configuration option `sync-cmd` as shell command."
]

