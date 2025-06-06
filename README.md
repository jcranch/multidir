# Multidir ("`muld`"): run tasks in multiple directories

This application exists to run tasks in several directories
sequentially. The motivating use is to view the status of, or fetch
remote changes to, multiple version control repositories.

The design differs from some other superficially similar applications
in that it is designed to be flexible: if you maintain a set of
repositories using several different version control systems,
requiring completely different commands to achieve analogous ends in
each directory, one can easily do so (and, in particular, there is no
built-in git-centric behaviour).

## Getting started (without explanations)

1. Clone the repository, and run `stack install` in it.
2. Copy `tasks.toml` and `recipes.toml` from `examples/` to
   `~/.config/muld/`.
3. Run `muld discover` on the directory containing all your projects.
4. Inspect `~/.config/muld/projs.toml` by hand to remove anything
   unwanted, and to add extra tags as desired.
5. Now you can run `muld status`, and `muld fetch` to fetch changes.

Steps 1 and 2 above can be replaced by:

1. Run `cabal update` and `cabal install multidir`.
2. Copy and paste `tasks.toml` and `recipes.toml` from the repository
   on Github to `~/.config/muld`.

## Getting started (with explanations)

You can build it from source by cloning the repository and running
`stack install` in it. This of course requires you to have [installed
Haskell/stack](https://www.haskell.org/ghcup/).

Configuration consists of providing three
[TOML](https://toml.io/en/v1.0.0) files:

* `projs.toml` lists the directories to be acted upon (together with
  tags which classify them).

* `tasks.toml` lists the tasks which may be run upon those
  directories, and explains how to do them; differing implementations
  can be given for differing tags.

* `recipes.toml` is used for autodiscovery; it contains instructions
  on what makes a directory suitable for discovery.

Vaguely useful samples of `recipes.toml` and `tasks.toml` can be found
in the `examples/` directory and copied in. Once `recipes.toml` is in
place, one can generate a `projs.toml` by running `muld discover` (see
below).

These configuration files are each searched for in the following
order:

1. Their locations can be supplied on the command-line (see below);
2. Failing that, their locations are deduced from the environment
   variables `$MULD_TASK_FILE` `$MULD_PROJ_FILE` and
   `$MULD_RECIPE_FILE`, if set;
3. Failing that, their locations are taken as `tasks.toml`,
   `projs.toml` and `recipes.toml` in `$XDG_CONFIG_HOME/muld` (if that
   environment variable is set);
4. Failing that, their locations are taken as `tasks.toml`,
   `projs.toml` and `recipes.toml` in `$HOME/.config/muld` (if that
   environment variable is set);
5. Failing that, an error is raised.

For most users, steps 3 and 4 have the same results.

## Command-line syntax

The following commands are built in. In every case there is
command-line assistance (type `muld COMMAND --help`):

* `muld discover` finds all subdirectories of the given directory
  matching the patterns in `recipes.toml`, and adds them to
  `projs.toml`.

* `muld register` adds the given directory to `projs.toml`.

* `muld list` lists the directories in `projs.toml`.

* `muld run` runs a supplied command in every directory.

* `muld where` list the locations of config files (useful for
  debugging, perhaps).

In addition, every task in `tasks.toml` gives another command which
can be called; that task is run in every directory.

## Selecting directories

Those commands listed above which operate on a collection of
directories (`list`, `run` and the task commands) can be put to work
on a user-defined set of directories. By default, it is those with the
`default` tag, but this can be adjusted using the `-s` flag.

The syntax is as follows:

* `muld list -s important` will list those repositories with the
  `important` tag;

* `muld list -s {/home/fred}` will list those repositories which are
  under `/home/fred`;

* these can be combined with the binary operators `&` (and), `|` (or)
  and, `\` (difference). Hence `muld list -s important\scary` will
  list the repositories which are `important` but not `scary`;

* parentheses are permitted for complex expressions;

* for ease, if a specification starts with an operator, it is treated
  as though `default` was specified first. So `muld list -s
  &interesting` lists the directories which are both `default` and
  `interesting`.

There is an `all` tag, which by default is added to all
repositories. One can remove the `all` tag from a repository, if
desired, but maybe that's not a very good idea.

## Defining tasks

User-defined tasks are in `tasks.toml`. They are listed by name, and
by what tag they operate on (allowing the same task to be run
differently for different types of directory).

The rule is that, for each directory, the tags are gone through in
order; the first tag with an implementation for that task is the one
which is used.

Here's an example:

    [fetch]
    description = "Downloads changes"
    [fetch.git]
    run = "git -c color.ui=always fetch"
    [fetch.jj]
    run = "TERM=xterm-color jj git fetch"

This defines a task which can be run on any directory with tags `git`
or `jj`.

## Defining recipes

Recipes for registering directories are in `recipes.toml`. Each recipe
consists of:

* the files required to trigger that recipe;

* the tags added if that recipe is triggered;

* the `discover` flag (which defaults to `True`) which says whether
  this recipe is enough on its own to discover a directory (or merely
  add extra tags to it if discovered for other reasons).

## General advice

In most setups, the default shell is not a login shell (`dash` rather
than `bash` under Debian, for example), and has not had read `.bashrc`
or anything. Hence one might need to be careful in defining tasks:

* specifying paths exactly if they're in `$PATH` in a login shell but
  not in the default shell;

* reassuring processes that they're okay to assume colour features for
  output (for example, prefacing jujutsu commands with
  `TERM=xterm-color`, and replacing `git` with `git -c
  color.ui=always`).

## Prior art

There are many other applications which do vaguely similar jobs. Examples
include:

* [Mani](https://alajmovic.com/posts/many-repo-cli-tool-in-go/) is
  heavyweight, fairly git-centric, and written in go.

* [Myrepos](https://myrepos.branchable.com/) is nice, but old and
  unloved; it is written in javascript for node.js.

## Development

The projects in [TODO.md](TODO.md) are probably all achievable, and I
will do them when I can be bothered (or when someone else can be
bothered).

I'd be happy to take suggestions for extra recipes or extra
implementations of tasks. I'm happy to take suggestions of extra tasks
(and if they're a bit specialist in nature, I may start a new
directory containing suggestions for more niche tasks).

Several of the other available applications support parallelism, which
we don't (at least not yet, and it's not high on my list of
priorities).

Bug reports are very welcome (just email me); there probably are some!
