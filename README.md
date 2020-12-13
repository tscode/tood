# Td and Tood

**Tood** is a simple ocaml library for parsing, manipulating, filtering and
formatting todo list entries. **Td** is a command line tool that builds upon
Tood, aiming to make it comfortable to manage todo lists on the terminal.

Noteworthy features of **td**:
* Todo lists are stored in a simple human readable and editable format.
* Can manage as many separate todo lists as you wish.
* Supports project/subproject, context, and date tags for entries.
* Powerful filters (regex, wildcards, ranges, ...) supporting arbitrary boolean conjunctions (and, or, not).
* Customizable entry formatting (custom format strings, ANSI terminal color support).
* Easy modification of entries by command (*td mod*) or by editing files (*td edit*).
* Handles even large lists (10,000 entries) swiftly and without issues.
* Meaningful error messages (<- maybe this is wishful thinking...)

A typical sequence of `td` interactions might look like this:

```bash
# Initialize td, generating a default config file at ~/.config/td/config
$ td init

# Append a new task with the context tag 'groceries' to the todo list
$ td add have to go shopping +groceries 

# Append an important task with context and project tags
$ td add ! email to colleague X +work +project/subproject

# Append a task with date tag
$ td add make appointment with doctor +health +2021-05-03

# List the active entries
$ td ls
1 - have to go shopping +groceries
2 ! email to colleague +work +project/subproject
3 - add make appointment with doctor +health +2021-05-03

# Selectively list the active entries
$ td ls +work
2 ! email to colleague X +work +project/subproject

# Move a task to the file with done entries
$ td do 2

# Equivalently, you could have typed
$ td do +work
$ td do email
$ td do !
$ td do '"colleague X"'
$ td do +word and email and ! and '"colleague X"'

# If your selection matches multiple entries, td by default asks
# if you are serious about your choice
$ td do 1..2
You are about to mark 2 entries as done. Proceed? [Y/n] n
info: action aborted

# List the tasks you have completed
$ td ls --done
2 ! email to colleague X +work +project/subproject
```

This project is useable and stable in its current state. The author relies on it
on a daily basis and plans to maintain and extend it in the future. Still, it is
mainly the product of an autodidactic impulse to get used to programming in the
great language ocaml.
If you consider using a terminal application to manage your todo lists, you
should at least also have a look at more serious and wholesome efforts, like
[todo.txt](http://todotxt.org/).

## Installation
Currently, installation by [opam](https://opam.ocaml.org/) is the only supported
option. Since `tood` and `td` are not (yet) registered at the official opam
repository, you can instead pin it:
```
opam pin git@github.com:tscode/tood.git
```
After that, `td` should be accessible from your shell whenever the correct opam
environment is set.


## Configuration

Configuration options are documented under `td init --help`.

## Usage
The usage of `td` is fully documented in the help pages of `td` and its
sub-commands. Currently, it supports the commands `init`, `ls`, `add`, `mod`,
`do`, `undo`, `rm`, `sort`, `lists`, and `sync`. When in doubt, type
`td --help` and `td init --help` to get some explanations how `td` works.

```
td init
```
Generates a default configuration file at the path `~/.config/td/config` or
`$TD_CONFIG`, if defined. It will ask you for the name of the default todo list
and the location of the todo- and done-files that are used for managing the
default list.

```
td add [prio] task text [tags...]
```
Adds a new task to your todo list. The optional argument `[prio]` is
the priority of the task and can be `?`, `-`, or `!` for 'low', 'medium', or
'high'. The argument `[tags...]` is a space separated list of tags. Tood
supports context tags (like `+cooking`), project tags containing `/`
(`+project/` or `+project/subproject`), and date tags (`+2055-05-03`).

```
td ls [filter]
```
Lists the entries of your todo list. The format of the entries when printing is
configurable. The argument `filter` determines which entries are printed.
Logical operations to negate or combine filters by `not`, `and` and `or` are
supported. See `td init --help` for more documentation about filters.

```
td mod filter [prio] [task text] [tag modifications...]
```
Modifies selected entries. You can change the priority or the task text or add
/ remove tags (e.g., via `+tobeadded` or `~toberemoved`). The order in which the
modifications are provided is not important, except that later mods override
earlier ones.

```
td do filter
td undo filter
```
Move selected entries to / from the done-file.

```
td rm filter
```
Removes selected entries from the todo list.

```
td sort [order]
```
Sorts the entries in the todo list according to different properties, like the
priority or the tags.

```
td lists
```
Prints all todo lists that are managed in the active configuration.

```
td sync
```
Executes a custom (synchronization) command that can be specified in the
configuration file.

## Todo
* An interactive mode that makes applying multiple changes more convenient and
  efficient.
* Export functionality. We want to be able to export to `todo.txt` and we would
  also like to be able to generate `.ics` files.

## Caveats
* All operations modifying todo lists currently rewrite the whole todo file.
  This is certainly not optimal, but it is sufficiently fast on a modern SSD
  even for huge lists.
* Td is used by very few people and might contain surprising bugs. So don't be
  shocked if it gets hungry and eats your todo lists alive.

