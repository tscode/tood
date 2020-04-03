# Td and Tood

*Tood* is a simple ocaml library for parsing, manipulating, filtering and
formatting todo list entries. *Td* is a command line tool that builds on
*Tood*, and which aims to make it comfortable to manage your todo list on the
terminal. It maintains two files with human readable todo list entries, one for
active entries and one for done ones. A typical sequence of `td` interactions
might look like this:

```
$ # Append a new task with the context tag 'groceries' to the todo list
$ td add have to go shopping +groceries 
$ # Append an important task with context and project tag
$ td add ! email to my chef +work +projectA/subproject
$ # List the active entries
$ td ls
1 - have to go shopping +groceries
2 ! email to my chef +work +projectA/subproject
$ # Selectively list your entries
$ td ls +work
2 ! email to my chef +work +projectA/subproject
$ # Move a task to the file with done entries
$ td do 2
$ td ls
1 - have to go shopping +groceries
$ td ls --done
2 ! email to my chef +work +projectA/subproject
```

Right now, `td` is already a useful and (hopefully!) stable tool with many
features. The author uses it on a daily basis and plans to maintain and extend
it in the future. Still, it mainly grew out of an autodidactic impulse to learn
ocaml. So if you think about using a terminal application to manage your todo
list, you should at least also have a look at more serious and wholesome
efforts, like [todo.txt](http://todotxt.org/).

## Installation
Currently, installation by [opam](https://opam.ocaml.org/) is the only supported
option. Since `tood` and `td` are not (yet) registered at the official opam
repository, you can instead pin it:
```
opam pin git@github.com:tscode/tood.git
```
After that, `td` should be accessible from your shell whenever the correct opam
environment is set.

## Usage
The usage of `td` is comprehensively documented in the help pages of `td` and
its commands `init`, `ls`, `add`, `mod`, `do`, `undo`, `rm`, and `sync`. The
following description has a more annecdotal character.

```
td init
```
can be used to generate a default `td` configuration file (at
`~/.config/td/conf` by default). It will ask you for the location of the files
for active and completed entries. Configuration options are documented under `td
init --help`.

```
td add [prio] task text [tags...]
```
adds a new task to your (active) todo list. The optional argument `[prio]` is
the priority of the task and can be `?`, `-`, or `!` for 'low', 'medium', or
'high'. The `[tags...]` are space separated lists of starting with `+`. Tood
supports context tags (like `+cooking`), project tags containing `/`
(`+project/` or `+project/subproject`), and date tags (`+2055-05-03`).

```
td ls [filter]
```
lists your todo list as a flat list. The format of the printed entries is
configurable. By the flag `--tree` the entries can be listed as project tree.
The `filter` determines which entries are printed. It can be anything from
an index or content substring to a tag or even a date range. Also, it supports
logical operations to negate or combine filters by `not`, `and` and `or`. See
`td init --help` for more documentation.

```
td mod filter [prio] [task text] [tag modifications...]
```
modifies selected tasks. You can change the priority, the task text, add tags
(e.g., `+newtag`), or remove tags (`~oldtag`). The order in which the
modifications are provided is not important (except that later mods override
earlier ones if they address the same priority / text / tag).

```
td do filter
td undo filter
```
move selected tasks to / from the done-file that stores the completed entries.

```
td rm filter
```
removes selected tasks from the todo file (or its done counterpart if `--done`
is provided).

```
td sync
```
executes a custom (synchronization) command that can be specified in the
configuration file of `td`.

## Caveats
* Due to the usage of the `Base` and `Core` libraries, the `td` binary is
  extremely large (>10 Mb!). I didn't anticipate this bloat. Eventually, this
  will be mended by bypassing first `Core` and then `Base`.
* A lot of things are not done terribly efficiently at the moment. Still, with
  a fast SSD drive, even todo lists with 10,000 entries appear to be
  maintainable with an acceptable performance (of about 1 second per usual td
  command) in testes.
* Td is only regularly used by very few people and might contain surprising
  bugs. So don't be shocked if it gets hungry and eats your todo lists alive.

