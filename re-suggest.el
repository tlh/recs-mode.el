;;; re-suggest.el -- a simple regexp-based command suggestion mode

;; Copyright (C) 2010 tlh <thunkout@gmail.com>

;; File:      re-suggest.el
;; Author:    tlh <thunkout@gmail.com>
;; Created:   2010-07-29
;; Version:   0.1
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

;; Commentary:
;;
;; Say you discover a new, more efficient sequence of commands to do
;; something. You want to start using it, but your muscle memory
;; continues to do it the old way. Before long you forget the new way,
;; and continue doing things the old way. With re-suggest, emacs can
;; recognize the old pattern when you use it, and suggest the new the
;; pattern, training you to be a better emacs user.
;;
;; re-suggest.el is a simple command suggestion minor-mode. It works
;; by mapping commands to characters, creating a string out of the
;; character mappings of recent commands, matching the resulting
;; string against a list of user-defined regexps that correspond to
;; command sequences, and finally suggesting a better way to do things
;; when a match is found. re-suggest looks strictly at sequences of
;; commands, not sequences of keystrokes, avoiding complications
;; resulting from different keybindings in different modes. This is
;; considered a feature.
;;
;; The advantage of this approach is that we can match any command
;; sequence that a regexp is powerful enough to match.
;;
;; The disadvantage of this approach is that we can only match command
;; sequences that a regexp is powerful enough to match.

;; Features:
;;
;;  - Definition of command patterns to be warned about as standard
;;    emacs regexps
;;
;;  - A timer to set the minimum interval between suggestions, per the
;;    emacs TODO list ("C-h C-t") guideline.
;;

;; Installation:
;;
;;  - put `re-suggest.el' somewhere on your emacs load path
;;
;;  - add these lines to your .emacs file:
;;    (require 're-suggest)
;;    (re-suggest-mode t)
;;

;; Configuration:
;;
;;  - You'll need to set the mapping of commands to character strings
;;    to your liking. To do so, set the value of
;;    `re-suggest-cmd-char-alist' to an alist of command to
;;    character-string mappings like so:
;;
;;    (setq re-suggest-cmd-char-alist
;;      '((newline         . "l")
;;        (previous-line   . "p")
;;        (next-line       . "n")))
;;
;;  - You'll also need to set the mapping of command sequence regexps
;;    to suggestion messages. To do so, set the value of
;;    `re-suggest-regexp-cmd-seq-alist' to an alist of
;;    command-sequence-regexp to suggestion-message mappings like so:
;;
;;    (defvar re-suggest-regexp-cmd-seq-alist
;;      '(("lpe" . "You should use `open-line' to do that.")
;;        ("ov"  . "You should use `scroll-other-window' to do that.")))
;;
;;    Any command names quoted `like-this' in suggestion messages will
;;    have their keybindings included on separate lines in the
;;    suggestion message.
;;
;;  - To set the maximum length of the command sequences re-suggest
;;    can recognize:
;;
;;    (setq re-suggest-cmd-string-length foo)
;;
;;  - You can set the minimum number of seconds between suggestions by
;;    setting `re-suggest-suggestion-interval':
;;
;;    (setq re-suggest-suggestion-interval 60)
;;
;;    If set to nil, suggestions will be made for every match.
;;

;; TODO:
;;
;;  - Add more suggestions.
;;  - Add mode specific command regexp matching.
;;

;;; Code:

(eval-when-compile
  (require 'cl))

(defvar re-suggest-null-cmd-char "_"
  "Character string used to represent commands no defined in
  `re-suggest-cmd-char-alist'.")

;; These commands won't be in any order that makes sense. I assigned
;; characters to them mnemonically at the beginning, before running
;; out of good ones, then alphabetized based on those characters to
;; better see which characters were available.
(defvar re-suggest-cmd-char-alist
  '((move-beginning-of-line      . "a")
    (backward-char               . "b")
    (backward-word               . "B")
    (set-mark-command            . "c")
    (delete-window               . "d")
    (move-end-of-line            . "e")
    (forward-char                . "f")
    (forward-word                . "F")
    (kill-ring-save              . "g")
    (indent-for-tab-command      . "i")
    (kill-line                   . "k")
    (newline                     . "l")
    (next-line                   . "n")
    (other-window                . "o")
    (previous-line               . "p")
    (beginning-of-buffer         . "r")
    (eval-last-sexp              . "s")
    (kill-word                   . "t")
    (backward-kill-word          . "T")
    (scroll-up                   . "v")
    (scroll-down                 . "V")
    (end-of-buffer               . "w")
    (forward-paragraph           . "x")
    (yank                        . "y")
    (backward-paragraph          . "z"))
  "An alist mapping commands to character strings. It's used to
convert sequences of commands into strings. The character string
defined in `re-suggest-null-cmd-char' is reserved for commands
not present in this list, and should not be used.

You should modify this list as you see fit.")

(defvar re-suggest-regexp-cmd-seq-alist
  '(("lpe"                       . "You should use `open-line' to do that.")
    ("zcx\\|xcz"                 . "You should use `mark-paragraph' to do that.")
    ("rcw\\|wcr"                 . "You should use `mark-whole-buffer' to do that.")
    ("xs"                        . "You should use `eval-defun' to do that.")
    ("li"                        . "You should use `newline-and-indent' to do that.")
    ("kkny"                      . "You should use `transpose-lines' to do that.")
    ("ov"                        . "You should use `scroll-other-window' to do that.")
    ("FFT"                       . "You should use `kill-word' to do that.")
    ("BBt"                       . "You should use `backward-kill-word' to do that.")
    ("n\\{20\\}"                 . "You should use more efficient navigation, like `forward-paragraph'.")
    ("p\\{20\\}"                 . "You should use more efficient navigation, like `backward-paragraph'.")
    ("f\\{20\\}"                 . "You should use more efficient navigation, like `forward-word'.")
    ("b\\{20\\}"                 . "You should use more efficient navigation, like `backward-word'."))
  "An alist mapping command sequence regexps to suggestion
  messages.

You should modify this list as you see fit.")

(defvar re-suggest-last-suggestion-time nil
  "System seconds at which the last suggestion occured.")

(defvar re-suggest-suggestion-interval nil
  "Minimum number of seconds between suggestions. If nil, no time
  checking is performed")

(defun re-suggest-current-time ()
  "Returns the current system time in seconds."
  (let ((time (current-time)))
    (+ (* (car time) (expt 2 16))
       (cadr time)
       (/ (caddr time) 1000000.0))))

(defun re-suggest-check-time ()
  "Returns t if `re-suggest-suggestion-interval' is nil, if
`re-suggest-last-suggestion-time' is nil or if the sum of the
previous two has been superceded, nil otherwise."
  (or (not re-suggest-suggestion-interval)
      (not re-suggest-last-suggestion-time)
      (>=  (re-suggest-current-time)
           (+ re-suggest-last-suggestion-time
              re-suggest-suggestion-interval))))

(defun re-suggest-record-time ()
  "Sets `re-suggest-last-suggestion-time' with the current time
in seconds."
  (setq re-suggest-last-suggestion-time (re-suggest-current-time)))

(defvar re-suggest-cmd-string nil
  "A string composed of characters that map to commands in
  `re-suggest-cmd-char-alist'.")

(defvar re-suggest-cmd-string-length 100
  "Length of `re-suggest-cmd-string'.")

(defun re-suggest-reset-cmd-string ()
  "Makes and empty cmd-string of length
`re-suggest-cmd-string-length'."
  (setq re-suggest-cmd-string
        (make-string re-suggest-cmd-string-length ? )))

(defun re-suggest-verify-cmd-string ()
  "Verifies that `re-suggest-cmd-string' exists, is a string, and
is of the length `re-suggest-cmd-string-length'."
  (unless (and re-suggest-cmd-string
               (stringp re-suggest-cmd-string)
               (= (length re-suggest-cmd-string)
                  re-suggest-cmd-string-length))
    (re-suggest-reset-cmd-string))
  re-suggest-cmd-string)

(defun re-suggest-record-cmd ()
  "Appends to `re-suggest-cmd-string' the character that
  `this-original-command' maps to in `re-suggest-cmd-char-alist',
  or \" \" if no match exists."
  (re-suggest-verify-cmd-string)
  (setq re-suggest-cmd-string
        (concat (subseq re-suggest-cmd-string 1)
                (or (cdr (assoc this-original-command re-suggest-cmd-char-alist))
                    re-suggest-null-cmd-char))))

(defun re-suggest-detect-match ()
  "Attempts to match `re-suggest-cmd-string' against all the
regexps in `re-suggest-regexp-cmd-seq-alist'. Returns the
corresponging message if a match is found, nil otherwise."
  (catch 'result
    (let (case-fold-search)
      (dolist (seq re-suggest-regexp-cmd-seq-alist nil)
        (and (string-match (car seq) re-suggest-cmd-string)
             (throw 'result (cdr seq)))))))

(defun re-suggest-extract-quoted (str)
  "Extract and intern a list of strings quoted like `this' from
STR."
  (let ((pos 0) (acc))
    (while (string-match "`\\(.*?\\)'" str pos)
      (push (intern (match-string 1 str)) acc)
      (setq pos (match-end 1)))
    (nreverse acc)))

(defun re-suggest-get-bindings (msg)
  "Calls `re-suggest-extract-quoted' to extract the quoted
  command names from MSG, then builds a list of bindings for each
  of these commands. Returns a list of command and binding
  strings."
  (mapcar (lambda (cmd)
            (let ((keys (where-is-internal (or (command-remapping cmd) cmd))))
              (cond ((not (commandp cmd))
                     (format "%s is actually not a command." cmd))
                    (keys
                     (concat (format "%s is bound to " cmd)
                             (mapconcat 'key-description keys ", ")))
                    (t (format "%s has no keybindings." cmd)))))
          (re-suggest-extract-quoted msg)))

(defun re-suggest-hook ()
  "Hook function to add to `post-command-hook'."
  (when (re-suggest-check-time)
    (re-suggest-record-cmd)
    (let ((msg (re-suggest-detect-match)))
      (when msg
        (message (mapconcat 'identity (cons msg (re-suggest-get-bindings msg)) "\n"))
        (ding)
        (re-suggest-reset-cmd-string)
        (re-suggest-record-time)))))

(defun re-suggest-enable (enable)
  "Enables `re-suggest-mode' when ENABLE is t, disables
otherwise."
  (cond (enable (add-hook 'post-command-hook 're-suggest-hook)
                (re-suggest-reset-cmd-string)
                (setq re-suggest-mode t))
        (t      (remove-hook 'post-command-hook 're-suggest-hook)
                (setq re-suggest-mode nil))))

;;;###autoload
(define-minor-mode re-suggest-mode
  "Toggle re-suggest minor mode.

If ARG is null, toggle re-suggest.
If ARG is a number greater than zero, turn on re-suggest.
Otherwise, turn off re-suggest."
  :lighter     " res"
  :init-value  nil
  :global      t
  (cond (noninteractive   (re-suggest-enable nil))
        (re-suggest-mode  (re-suggest-enable t))
        (t                (re-suggest-enable nil))))

(provide 're-suggest)
