;;; recs-mode.el --- [R]eg[E]xp-based [C]ommand [S]uggestion

;; Copyright (C) 2010 tlh <thunkout@gmail.com>

;; File:      recs-mode.el
;; Author:    tlh <thunkout@gmail.com>
;; Created:   2010-07-29
;; Version:   1.0
;; Keywords:  command suggestion regexp
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA
;;

;;; Commentary:
;;
;; recs-mode is a simple command suggestion minor mode for GNU
;; Emacs. recs-mode is free software, licensed under the GNU GPL.
;;
;; The latest version of recs-mode can always be found here:
;;
;;     http://github.com/tlh/recs-mode.el
;;
;; You can clone the repo by running:
;;
;;     git clone git://github.com/tlh/recs-mode.el
;;
;;
;; Say you discover a new, more efficient sequence of commands to do
;; something.  You want to start using it, but your muscle memory
;; continues to do it the old way.  Before long you forget the new
;; way, and nothing changes.  With recs-mode, Emacs can recognize the
;; old pattern when you use it, and suggest the new the pattern,
;; training you to be a better Emacs user.
;;
;; recs-mode works by recording the names of the commands you enter
;; into a ring-like string, which is then matched against a list of
;; command name regular expressions.  When a match is detected,
;; recs-mode suggests a more efficient way of doing things.  recs-mode
;; looks strictly at sequences of commands, not sequences of
;; keystrokes, avoiding complications resulting from different
;; keybindings in different modes.
;;
;; The advantage of this approach is that we can detect any command
;; sequence that a regexp is powerful enough to match.
;;
;; The disadvantage of this approach is that we can only detect
;; command sequences that a regexp is powerful enough to match.
;;

;;; Features:
;;
;;  - Definition of command patterns as standard Emacs regexps
;;
;;  - Suggestions can be sent to the echo area or to another window.
;;    Having the window selected out from under you can get really
;;    annoying, making it desirable to learn quickly.
;;
;;  - A timer to set the minimum interval between suggestions, per the
;;    Emacs TODO list (C-h C-t) guidelines.
;;
;;  - A hook that's run whenever a match is detected. This can be used
;;    in conjunction with `recs-suppress-suggestion' and
;;    `recs-ding-on-suggestion' to define completely different
;;    behavior on match detection.

;;; Installation:
;;
;;  - put `recs-mode.el' somewhere on your Emacs load path
;;
;;  - add these lines to your `.emacs' file:
;;
;;         (require 'recs-mode)
;;         (recs-mode t)
;;

;;; Configuration:
;;
;;  - recs-mode comes with a number of default patterns, but you should
;;    modify these to fit your usage.  The default patterns are stored in
;;    the file `recs-patterns.el' that comes with recs-mode.  You should
;;    copy that file somewhere sensible, like `~/.emacs.d/', and
;;    customize it there.  You will need to set the value of
;;    `recs-pattern-file' to the new location:
;;
;;         (setq recs-pattern-file "/path/to/new/recs-patterns")
;;
;;    A recs-mode pattern consists of a list containing a command
;;    sequence regular expression, an expression that is evaluated to
;;    produce a suggestion message, and any number of command name
;;    symbols:
;;
;;         '("newline previous-line move-end-of-line"
;;           "This suggestion is just a string, but it could\
;;            be any lisp form that produces a string."
;;           open-line)
;;
;;    Patterns must be added to the list `recs-patterns'. You can use
;;    the convenience function `recs-add-pattern' to do that:
;;
;;         (recs-add-pattern
;;           (list "\\(some-command \\| some-other-command \\)+newline yank"
;;                 '(progn "You should use `better-command' to do that.")
;;                 'better-command))
;;
;;    Notice above that the suggestion expression is a progn form, not
;;    just a string.  If the form evaluates to nil, no suggestion is
;;    triggered.
;;
;;    Commands listed in suggestions should be quoted like `this',
;;    allowing emacs' help system to link to their docstrings.
;;    recs-mode will print the keybindings, if they exist, of the
;;    commands at the end of the list.  These should typically be the
;;    same ones that are quoted in the suggestion message.
;;
;;    After modifying the contents of `recs-pattern-file' you will need
;;    to reload the file for the changes to take effect.  You can do that
;;    either by toggling recs-mode off and on with two invocations of:
;;
;;         M-x recs-mode
;;
;;    or by issuing the command:
;;
;;         M-x recs-load-pattern-file
;;
;;  - Other customizable variables include:
;;
;;         recs-cmdstr-max
;;         recs-suggestion-interval
;;         recs-ding-on-suggestion
;;         recs-suggestion-window
;;         recs-window-select
;;         recs-mode-hook
;;         recs-suppress-suggestion
;;         recs-log-file
;;         recs-log-suggestions
;;         recs-buffer-name
;;
;;    See the documentation for these variables below, or enter:
;;
;;         C-u M-x customize-mode RET recs-mode RET

;;; Code:

(eval-when-compile
  (require 'cl))

;; Customization

(defgroup recs nil
  "The [R]egular [E]xpression-based [C]ommand [S]uggester
A command suggestion minor mode for GNU Emacs."
  :group 'help
  :version "1.0")

(defcustom recs-cmdstr-max 500
  "Max length of `recs-cmdstr'.
Increase this number in order to be able to detect longer
patterns of commands."
  :type 'integer
  :group 'recs)

(defcustom recs-suggestion-interval nil
  "Minimum number of seconds between suggestions.
If nil, no time checking is performed."
  :type 'boolean
  :group 'recs)

(defcustom recs-ding-on-suggestion t
  "Whether to `ding' when a suggestion is made."
  :type 'boolean
  :group 'recs)

(defcustom recs-suggestion-window nil
  "When nil, display suggestions in the echo area.
When t, display suggestions in a separate window.  Window
selection is defined by `recs-window-select'."
  :type 'boolean
  :group 'recs)

(defcustom recs-window-select nil
  "Behaves identically to `help-window-select'."
  :type 'symbol
  :group 'recs)

(defcustom recs-mode-hook nil
  "Hook run whenever a suggestion is triggered."
  :type 'hook
  :group 'recs)

(defcustom recs-suppress-suggestion nil
  "When nil, trigger suggestions normally.
When t, recs-mode doesn't actually make a suggestion when a match
is detected.  This can be used in conjunction with
`recs-mode-hook' to define new behavior on match."
  :type 'boolean
  :group 'recs)

(defcustom recs-log-file "~/.emacs.d/recs-log"
  "File to which `recs-mode' will log suggestions.
Logging occurs when `recs-log-suggestions' is non-nil."
  :type 'file
  :group 'recs)

(defcustom recs-log-suggestions t
  "When non-nil, log suggestions to `recs-log-file'.
When nil, do not."
  :type 'boolean
  :group 'recs)

(defcustom recs-pattern-file (locate-library "recs-patterns")
  "File from which `recs-mode' loads its pattern definitions."
  :type 'file
  :group 'recs)

(defcustom recs-buffer-name "*recs-mode-suggestions*"
  "Name of the `recs-mode' suggestion buffer."
  :type 'string
  :group 'recs)

;; Non-customizable variables

(defvar recs-patterns nil
  "List of `recs-mode' pattern definitions.
It's a list of lists of a command sequence regexp, a suggestion
form, and any number of command name symbols for which
keybindings will be printed.")

(defvar recs-cmdstr ""
  "String of names of the most recent commands.
Newly entered command names are appended to the end, including a
trailing space.  Its length will never exceed `recs-cmdstr-max'.")

(defvar recs-last-suggestion-time nil
  "System time (in seconds) when the last suggestion occured.")

;; Functions

(defun recs-add-pattern (pattern)
  "Convenience function to add PATTERN to `recs-patterns'.
It uses `add-to-list' so, only unique patterns are added."
  (add-to-list 'recs-patterns pattern))

(defun recs-current-time ()
  "Return abbreviated `current-time' in seconds."
  (cadr (current-time)))

(defun recs-check-time ()
  "Check that enough time has elapsed since the last suggestion.
Return t if `recs-suggestion-interval' is nil, if
`recs-last-suggestion-time' is nil or if the sum of the previous
two has been superceded. Otherwise return nil."
  (or (not recs-suggestion-interval)
      (not recs-last-suggestion-time)
      (>=  (recs-current-time)
           (+ recs-last-suggestion-time
              recs-suggestion-interval))))

(defun recs-record-time ()
  "Set `recs-last-suggestion-time' with `recs-current-time'."
  (setq recs-last-suggestion-time (recs-current-time)))

(defun recs-reset-cmdstr ()
  "Reset `recs-cmdstr' to the empty string."
  (setq recs-cmdstr ""))

(defun recs-record-cmd ()
  "Append `this-original-command' to `recs-cmdstr'.
The printed name of the command, including a trailing space, is
used.  Chop off the front of `recs-cmdstr' if it exceeds
`recs-cmdstr-max'."
  (let* ((new (format "%s " this-original-command))
         (str (concat recs-cmdstr new))
         (len (length str)))
    (setq recs-cmdstr (if (> len recs-cmdstr-max)
                          (subseq str (- len recs-cmdstr-max))
                        str))))

(defun recs-detect-match ()
  "Match `recs-cmdstr' against the regexps in `recs-patterns'.
If a match is found, and the suggestion form evaluates to
non-nil, return a list of the matched string, the regexp, the
result of the evaluation of the suggestion form, and any
command-name symbols from the end of the suggestion definition.
Keep in mind that `pattern' will be bound to the pattern
definition within the body of the suggestion form, if you need
values from it."
  (catch 'result
    (let (case-fold-search)
      (dolist (pattern recs-patterns)
        (when (string-match (car pattern) recs-cmdstr)
          (let ((sug (eval (nth 1 pattern))))
            (when sug
              (throw 'result
                     (append (list (match-string 0 recs-cmdstr)
                                   (car pattern)
                                   sug)
                             (cddr pattern))))))))))

(defun recs-princ-suggestion (match)
  "Generate from MATCH and princ the suggestion.
Bind `standard-output' to the desired destination before
calling."
  (flet ((mprinc (&rest args) (mapc 'princ args)))
    (mprinc "You entered the command sequence:\n\n["
            (car match) "]\n\n" (caddr match) "\n\n")
    (dolist (cmd (cdddr match))
      (let ((keys (where-is-internal (or (command-remapping cmd) cmd))))
        (cond ((not (commandp cmd))
               (mprinc "`" cmd "' is actually not a command."))
              (keys
               (mprinc "`" cmd "' is bound to: "
                       (mapconcat 'key-description keys ", ")))
              (t (mprinc "`" cmd "' has no keybindings."))))
      (princ "\n"))))

(defun recs-suggest (match)
  "Output the suggestion generated from MATCH.
Display it in a separate buffer if `recs-suggestion-window' is
non-nil, otherwise in the echo area.  Suggestion window selection
is configured with `recs-window-select'."
  (if recs-suggestion-window
      (let ((help-window-select recs-window-select))
        (with-help-window recs-buffer-name (recs-princ-suggestion match)))
    (message (with-output-to-string (recs-princ-suggestion match)))))

(defun recs-log-suggestion (match)
  "Log MATCH to `recs-log-fle'."
  (with-temp-buffer
    (let (make-backup-files)
      (insert (format "%S\n" match))
      (append-to-file (point-min) (point-max) recs-log-file))))

(defun recs-load-pattern-file (&optional file)
  "Load FILE or `recs-pattern-file'.
Set `recs-patterns' to nil first."
  (interactive)
  (setq recs-patterns nil)
  (load-library (or file recs-pattern-file)))

(defun recs-hook-fn ()
  "This is the hook function added to `post-command-hook'."
  (when (recs-check-time)
    (recs-record-cmd)
    (let ((match (recs-detect-match)))
      (when match
        (recs-reset-cmdstr)
        (recs-record-time)
        (run-hooks 'recs-mode-hook)
        (and recs-ding-on-suggestion (ding))
        (and recs-log-suggestions (recs-log-suggestion match))
        (unless recs-suppress-suggestion
          (recs-suggest match))))))

(defun recs-enable (enable)
  "Enable `recs-mode' when ENABLE is t, disable otherwise."
  (cond (enable
         (add-hook 'post-command-hook 'recs-hook-fn)
         (recs-reset-cmdstr)
         (recs-load-pattern-file)
         (setq recs-mode t))
        (t
         (remove-hook 'post-command-hook 'recs-hook-fn)
         (setq recs-mode nil))))

;; mode definition

;;;###autoload
(define-minor-mode recs-mode
  "This toggles recs-mode.

If ARG is null, toggle recs-mode.
If ARG is a number greater than zero, turn on recs-mode.
Otherwise, turn off recs-mode."
  :lighter     " Recs"
  :init-value  nil
  :global      t
  :group       'recs
  (cond (noninteractive (recs-enable nil))
        (recs-mode      (recs-enable t))
        (t              (recs-enable nil))))

(provide 'recs-mode)

;;; recs-mode.el ends here
