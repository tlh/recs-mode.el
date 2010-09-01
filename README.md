# recs-mode --- [R]eg[E]xp-based [C]ommand [S]uggestion

recs-mode is a simple command suggestion minor mode for GNU
Emacs. recs-mode is free software, licensed under the GNU GPL.

The latest version of recs-mode can always be found [here][]. You can
also clone the repo by running

    git clone git://github.com/tlh/recs-mode.el

 [here]: http://github.com/tlh/recs-mode.el

Say you discover a new, more efficient sequence of commands to do
something.  You want to start using it, but your muscle memory
continues to do it the old way.  Before long you forget the new way,
and nothing changes.  With recs-mode, Emacs can recognize the old
pattern when you use it, and suggest the new the pattern, training you
to be a better Emacs user.

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
   Emacs TODO list (C-h C-t) guidelines.

## Installation and Configuration

 - See the comments at the beginning of recs-mode.el for installation
   and configuration information.

## License

recs-mode is released under the GNU GPL. See `recs-mode.el`
