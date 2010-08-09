# recs - The [R]egular [E]xpression-based [C]ommand [S]uggester

recs-mode is a simple command suggestion minor mode for GNU
Emacs. recs-mode is free software, licensed under the GNU GPL.

The latest version of recs-mode can always be found [here][]. You can
also clone the repo by running

    git clone git://github.com/tlh/recs-mode.el

 [here]: http://github.com/tlh/recs-mode.el

Say you discover a new, more efficient sequence of commands to do
something.  You want to start using it, but your muscle memory
continues to do it the old way.  Before long you forget the new way,
and continue doing things the old way.  With recs-mode, Emacs can
recognize the old pattern when you use it, and suggest the new the
pattern, training you to be a better Emacs user.

recs-mode works by recording the names of the commands you enter into
a ring-like string, which is then matched against a list of command
name regular expressions.  When a match is detected, recs-mode
suggests a more efficient way of doing things.  recs-mode looks
strictly at sequences of commands, not sequences of keystrokes,
avoiding complications resulting from different keybindings in
different modes.

The advantage of this approach is that we can detect any command
sequence that a regexp is powerful enough to match.

The disadvantage of this approach is that we can only detect command
sequences that a regexp is powerful enough to match.

## Features

 - Definition of command patterns as standard Emacs regexps

 - Suggestions can be sent to the echo area or to another window.
   Having the window selected out from under you can get really
   annoying, making it desirable to learn quickly.

 - A timer to set the minimum interval between suggestions, per the
   Emacs TODO list (`C-h C-t`) guidelines.

 - A hook that's run whenever a match is detected. This can be used in
   conjunction with `recs-suppress-suggestion` and
   `recs-ding-on-suggestion` to define completely different behavior
   on match detection.

## Installation

 - put `recs.el` somewhere on your Emacs load path

 - add these lines to your `.emacs` file:

        (require 'recs)
        (recs-mode t)

## Configuration

 - recs-mode comes with a number of default patterns, but you should
   modify these to fit your usage.  The default patterns are stored in
   the file `recs-patterns` that comes with recs-mode.  You should
   copy that file somewhere sensible, like `~/.emacs.d/`, and
   customize it there.  You will need to set the value of
   `recs-pattern-file` to the new location:

        (setq recs-pattern-file "/path/to/new/recs-patterns")

   A recs-mode pattern consists of a list containing a command
   sequence regular expression, a suggestion message, and any number
   of command name symbols:

        ("newline previous-line move-end-of-line"
         "You should use `open-line' to do that."
         open-line)

        ("\\(some-command \\| some-other-command \\)+newline yank"
         "You should use `better-command' or `even-better-command'
          to do that."
         better-command
         even-better-command)

   Commands listed in suggestions should be quoted like `\`this'`,
   allowing emacs' help system to link to its documentation.
   recs-mode will print the keybindings, if they exist, of the
   commands at the end of the list.  These should typically be the
   same ones that are quoted in the suggestion message.

   After modifying the contents of `recs-pattern-file` you will need
   to reload the file for the changes to take effect.  You can do that
   either by toggling recs-mode off and on with two invocations of:

        M-x recs-mode

   or by issuing the command:

        M-x recs-load-pattern-file

 - Other customizable variables include:

        recs-cmdstr-max
        recs-suggestion-interval
        recs-ding-on-suggestion
        recs-suggestion-window
        recs-window-select
        recs-hook
        recs-suppress-suggestion
        recs-log-file
        recs-log-suggestions

   See the documentation for these variables in `recs.el`, or
   enter:

        C-u M-x customize-mode RET recs-mode RET

## License

recs-mode is released under the GNU GPL. See `recs.el`
