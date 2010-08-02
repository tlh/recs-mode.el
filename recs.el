;;; recs.el --- [R]egular [E]xpression-based [C]ommand [S]uggester

;; Copyright (C) 2010 tlh <thunkout@gmail.com>

;; File:      recs.el
;; Author:    tlh <thunkout@gmail.com>
;; Created:   2010-07-29
;; Version:   1.0
;; Keywords:  command suggestion regexp

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Commentary:
;;
;; Say you discover a new, more efficient sequence of commands to do
;; something.  You want to start using it, but your muscle memory
;; continues to do it the old way.  Before long you forget the new
;; way, and continue doing things the old way.  With recs, Emacs can
;; recognize the old pattern when you use it, and suggest the new the
;; pattern, training you to be a better Emacs user.
;;
;; recs.el is a simple command suggestion minor-mode.  It works by
;; logging the names of the commands you enter to ring-like string
;; which is then matched against a list of command-sequence regexps.
;; When a match is detected recs suggests a more efficient way of
;; doing things.  recs looks strictly at sequences of commands, not
;; sequences of keystrokes, avoiding complications resulting from
;; different keybindings in different modes.  This is considered a
;; feature.
;;
;; The advantage of this approach is that we can match any command
;; sequence that a regexp is powerful enough to match.
;;
;; The disadvantage of this approach is that we can only match command
;; sequences that a regexp is powerful enough to match.
;;

;;; Features:
;;
;;  - Definition of command patterns as standard Emacs regexps
;;
;;  - Suggestions can be sent to the echo area or to another window.
;;    Having the window selected out from under you can get really
;;    annoying, making it desirable to learn quickly.
;;
;;  - A hook that's run whenever a match is detected.
;;
;;  - A timer to set the minimum interval between suggestions, per the
;;    Emacs TODO list ("C-h C-t") guidelines.
;;
;;  - Togglable `ding'
;;

;;; Installation:
;;
;;  - put `recs.el' somewhere on your Emacs load path
;;
;;  - add these lines to your .emacs file:
;;    (require 'recs)
;;    (recs-mode t)
;;

;;; Configuration:
;;
;;  - recs comes with a number of default patterns, but you should
;;    modify these to fit your usage.  The value of `recs-cmd-regexps'
;;    should be a list of lists of a command-sequence-regexp, a
;;    suggestion-message and any number of command name symbols like
;;    so:
;;
;;    (defvar recs-cmd-regexps
;;      '(("newline previous-line move-end-of-line"
;;         "You should use `open-line' to do that."
;;         open-line)
;;        ("newline indent-for-tab-command"
;;         "You should use `newline-and-indent' to do that."
;;         newline-and-indent
;;         some-other-command)))
;;
;;    recs will print the keybindings of the commands listed at the
;;    end, if keybindings for them exist.
;;
;;  - Other customizable variables include:
;;
;;    `recs-cmdstr-length'
;;    `recs-suggestion-interval'
;;    `recs-ding-on-suggestion'
;;    `recs-suggestion-window'
;;    `recs-cmdstr-length'
;;    `recs-window-select'
;;    `recs-hook'
;;    `recs-suppress-suggestion'
;;
;;    See the documentation for these variables below.
;;

;;; TODO:
;;
;;  - More default suggestions
;;  - Log matched patterns
;;  - Interactive pattern definition
;;  - Separate config file
;;

;;; Code:

(eval-when-compile
  (require 'cl))

;; Customize

(defgroup recs nil
  "[R]egular [E]xpression-based [C]ommand [S]uggester: a command
suggestion minor mode."
  :group 'help
  :version "1.0")

(defcustom recs-cmdstr-length 500
  "Length of `recs-cmdstr'.  Increase this number in order to be
able to detect longer patterns of commands."
  :type 'integer
  :group 'recs)

(defcustom recs-suggestion-interval nil
  "This defines the minimum number of seconds between
suggestions.  If nil, no time checking is performed."
  :type 'boolean
  :group 'recs)

(defcustom recs-ding-on-suggestion t
  "Defines whether to call `ding' when a suggestion is made."
  :type 'boolean
  :group 'recs)

(defcustom recs-suggestion-window nil
  "NIL means display suggestion in the echo area.  t means
display suggestion in a separate window.  Window selection is
defined by `recs-window-select'."
  :type 'boolean
  :group 'recs)

(defcustom recs-window-select nil
  "Acceptable values correspond to those for
`help-window-select'."
  :type 'symbol
  :group 'recs)

(defcustom recs-hook nil
  "Hook run whenever a suggestion is triggered."
  :type 'hook
  :group 'recs)

(defcustom recs-suppress-suggestion nil
  "NIL means normal suggestions behavior.  t means recs won't
actually make a suggestion when a match is found.  This can be
used in conjunction with `recs-hook' to define your own behavior
on match."
  :type 'boolean
  :group 'recs)

(defcustom recs-cmd-regexps
  '(("newline previous-line move-end-of-line"
     "You should use `open-line' to do that."
     open-line)
    ("newline indent-for-tab-command"
     "You should use `newline-and-indent' to do that."
     newline-and-indent)
    ("forward-paragraph eval-last-sexp"
     "You should use `eval-defun' to do that."
     eval-defun)
    ("other-window scroll-up"
     "You should use `scroll-other-window' to do that."
     scroll-other-window)
    ("other-window scroll-down"
     "You should use `scroll-other-window-down' to do that."
     scroll-other-window-down)
    ("move-beginning-of-line indent-for-tab-command"
     "You should use `back-to-indentation' to do that."
     back-to-indentation)
    ("other-window \\(find-file\\|ido-find-file\\)"
     "You should use `find-file-other-window' to do that."
     find-file-other-window)
    ("kill-line \\(kill-line \\|delete-char \\)next-line yank"
     "You should use `transpose-lines' to do that."
     transpose-lines)
    ("\\(beginning-of-defun set-mark-command \\(end-of-defun\\|forward-sexp\\)\\|end-of-defun set-mark-command \\(beginning-of-defun\\|backward-sexp\\)\\)"
     "You should use `mark-defun' to do that."
     mark-defun)
    ("\\(backward-paragraph set-mark-command forward-paragraph\\|forward-paragraph set-mark-command backward-paragraph\\)"
     "You should use `mark-paragraph' to do that."
     mark-paragraph)
    ("\\(beginning-of-buffer set-mark-command end-of-buffer\\|end-of-buffer set-mark-command beginning-of-buffer\\)"
     "You should use `mark-whole-buffer' to do that."
     mark-whole-buffer)
    ("mark-paragraph kill-region \\(backward-paragraph \\|forward-paragraph \\)+yank"
     "You should use `transpose-paragraphs' to do that."
     transpose-paragraphs)
    ("set-mark-command \\(forward-list \\|backward-list \\)+kill-region \\(forward-list \\|backward-list \\)+\\(newline \\)*yank"
     "You should use `transpose-sexps' to do that."
     transpose-sexps)
    ("set-mark-command \\(forward-word \\|backward-word \\)kill-region \\(forward-word \\|backward-word \\)+yank"
     "You should use `transpose-words' to do that."
     transpose-words)
    )
  "A list of lists consisting of a command sequence regexp, a
suggestion message, and any number of command name symbols for
which the keybindings will be printed."
  :type 'alist
  :group 'recs)

;; Non-customizable variables

(defvar recs-cmdstr ""
  "This is a ring-like string composed of the names of the most
recently entered commands, with the recent being appended to the
end.  Its length should never exceed `recs-cmdstr-length'.")

(defvar recs-last-suggestion-time nil
  "System seconds at which the last suggestion occured.")

(defun recs-current-time ()
  "Return the current system time in seconds."
  (let ((time (current-time)))
    (+ (* (car time) (expt 2 16))
       (cadr time)
       (/ (caddr time) 1000000.0))))

(defun recs-check-time ()
  "Return t if `recs-suggestion-interval' is nil, if
`recs-last-suggestion-time' is nil or if the sum of the previous
two has been superceded, nil otherwise."
  (or (not recs-suggestion-interval)
      (not recs-last-suggestion-time)
      (>=  (recs-current-time)
           (+ recs-last-suggestion-time
              recs-suggestion-interval))))

(defun recs-record-time ()
  "Set `recs-last-suggestion-time' with the current time in
seconds."
  (setq recs-last-suggestion-time (recs-current-time)))

(defun recs-reset-cmdstr ()
  "Reset `recs-cmdstr' to the empty string."
  (setq recs-cmdstr ""))

(defun recs-record-cmd ()
  "Append to `recs-cmdstr' the print name of
`this-original-command' with a trailing space, and chop off the
front if it exceeds `recs-cmdstr-length'."
  (let* ((new (format "%s " this-original-command))
         (str (concat recs-cmdstr new))
         (len (length str)))
    (setq recs-cmdstr (if (> len recs-cmdstr-length)
                          (subseq str (- len recs-cmdstr-length))
                        str))))

(defun recs-detect-match ()
  "Attempt to match `recs-cmdstr' against all the regexps in
`recs-cmd-regexps'.  Return a list of the matched string, the
pattern and the suggestion message when a match is found, nil
otherwise."
  (catch 'result
    (let (case-fold-search)
      (dolist (pattern recs-cmd-regexps nil)
        (and (string-match (car pattern) recs-cmdstr)
             (throw 'result (cons (match-string 0 recs-cmdstr)
                                  pattern)))))))

(defun recs-princ-suggestion (match)
  "Generate from MATCH and princ the suggestion.  Bind
`standard-output' to the desired destination before calling."
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
  "Display the suggestion generated from MATCH in a separate
buffer if `recs-suggestion-window' is non-nil, otherwise in the
echo area.  Suggestion window selection is configured with
`recs-window-select'."
  (if recs-suggestion-window
      (let ((help-window-select recs-window-select))
        (with-help-window "*recs*" (recs-princ-suggestion match)))
    (message (with-output-to-string (recs-princ-suggestion match)))))

(defun recs-hook-fn ()
  "This is the hook function that gets added to
`post-command-hook'."
  (when (recs-check-time)
    (recs-record-cmd)
    (let ((match (recs-detect-match)))
      (when match
        (recs-reset-cmdstr)
        (recs-record-time)
        (run-hooks 'recs-hook)
        (and recs-ding-on-suggestion (ding))
        (or recs-suppress-suggestion
            (recs-suggest match))))))

(defun recs-enable (enable)
  "Enable `recs-mode' when ENABLE is t, disable otherwise."
  (cond (enable (add-hook 'post-command-hook 'recs-hook-fn)
                (recs-reset-cmdstr)
                (setq recs-mode t))
        (t      (remove-hook 'post-command-hook 'recs-hook-fn)
                (setq recs-mode nil))))

;;;###autoload
(define-minor-mode recs-mode
  "This toggles the recs-mode.

If ARG is null, toggle recs-mode.
If ARG is a number greater than zero, turn on recs-mode.
Otherwise, turn off recs-mode."
  :lighter     " Recs"
  :init-value  nil
  :global      t
  (cond (noninteractive   (recs-enable nil))
        (recs-mode        (recs-enable t))
        (t                (recs-enable nil))))

(provide 'recs)

;;; recs.el ends here
