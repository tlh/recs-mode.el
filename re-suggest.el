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
;; re-suggest.el is a simple command suggestion minor-mode. It works
;; by mapping commands to characters, creating a string out of the
;; character mappings of recent commands, matching the resulting
;; string against a list of user-defined regexps that correspond to
;; command sequences, and finally suggesting a better way to do things
;; when a match is found. re-suggest looks strictly at sequences of
;; commands, not sequences of keys, and so avoids most of the
;; complications resulting from different keybindings in different
;; modes. This is considered a feature.
;;
;; The advantage of this approach is that we can match any command
;; sequence that a regexp is powerful enough to match.
;;
;; The disadvantage of this approach is that we can only match any
;; command sequence that a regexp is powerful enough to match.

;; Features:
;;
;;  - Define command patterns about which to be warned as regexps
;;
;;  - Timer to set the minimum interval between suggestions per the
;;    emacs TODO list ("C-h C-t") suggestions.
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
;;  - Add a lot more suggestions. Use emacswiki's command suggestion
;;    page.
;;

;;; Code:

(eval-when-compile
  (require 'cl))

(defvar re-suggest-cmd-char-alist
  '((newline                . "l")
    (previous-line          . "p")
    (next-line              . "n")
    (forward-char           . "f")
    (backward-char          . "b")
    (forward-word           . "F")
    (backward-word          . "B")
    (kill-word              . "t")
    (backward-kill-word     . "T")
    (kill-line              . "k")
    (move-beginning-of-line . "a")
    (move-end-of-line       . "e")
    (other-window           . "o")
    (scroll-up              . "v")
    (scroll-down            . "V")
    (delete-window          . "d")
    (backward-paragraph     . "z")
    (forward-paragraph      . "x")
    (set-mark-command       . "c")
    (eval-last-sexp         . "s")
    (indent-for-tab-command . "i")
    (yank                   . "y"))
  "An alist mapping commands to character strings. It's used to
convert sequences of commands into strings. The \" \" character
is reserved for commands not present in this list.

You should modify this list as you see fit.")

(defvar re-suggest-regexp-cmd-seq-alist
  '(("lp"                        . "You should use `open-line' to do that.")
    ("[zx]c[xz]"                 . "You should use `mark-paragraph' to do that.")
    ("xs"                        . "You should use `eval-defun' to do that.")
    ("li"                        . "You should use `newline-and-indent' to do that.")
    ("kkny"                      . "You should use `transpose-lines' to do that.")
    ("ov"                        . "You should use `scroll-other-window' to do that.")
    ("FFT"                       . "You should use `kill-word' to do that.")
    ("BBt"                       . "You should use `backward-kill-word' to do that.")
    ("n\\{20\\}"                 . "You should use more efficient navigation, like forward-paragraph.")
    ("p\\{20\\}"                 . "You should use more efficient navigation, like backward-paragraph.")
    ("f\\{20\\}"                 . "You should use more efficient navigation, like forward-word.")
    ("b\\{20\\}"                 . "You should use more efficient navigation, like backward-word."))
  "An alist mapping command sequence regexps to suggestion
  messages.

You should modify this list as you see fit.")

(defvar re-suggest-last-suggestion-time nil
  "System seconds at which the last suggestion occured.")

(defvar re-suggest-suggestion-interval 10
  "Minimum number of seconds between suggestions. If nil, no time
  checking is performed")

(defun re-suggest-check-time ()
  "Sets `re-suggest-last-suggestion-time' with and returns the
current system time in seconds if
`re-suggest-suggestion-interval' is nil, if
`re-suggest-last-suggestion-time' is nil or if the sum of the
previous two has been superceded. Returns nil otherwise."
  (destructuring-bind (t0 t1 t2) (current-time)
    (let ((secs (+ (* t0 (expt 2 16)) t1 (/ t2 1000000.0))))
      (when (or (not re-suggest-suggestion-interval)
                (not re-suggest-last-suggestion-time)
                (>= secs (+ re-suggest-last-suggestion-time
                            re-suggest-suggestion-interval)))
        (setq re-suggest-last-suggestion-time secs)))))

(defvar re-suggest-cmd-string-length 100
  "Length of `re-suggest-cmd-string'.")

(defun re-suggest-make-empty-cmd-string ()
  "Makes and empty cmd-string of length
`re-suggest-cmd-string-length'."
  (make-string re-suggest-cmd-string-length ? ))

(defvar re-suggest-cmd-string (re-suggest-make-empty-cmd-string)
  "A string composed of characters that map to commands in
  `re-suggest-cmd-char-alist'.")

(defun re-suggest-verify-cmd-string ()
  "Verifies that `re-suggest-cmd-string' exists, is a string, and
is of the length `re-suggest-cmd-string-length'."
  (unless (and re-suggest-cmd-string
               (stringp re-suggest-cmd-string)
               (= (length re-suggest-cmd-string)
                  re-suggest-cmd-string-length))
    (setq re-suggest-cmd-string (re-suggest-make-empty-cmd-string)))
  re-suggest-cmd-string)

(defun re-suggest-record-cmd ()
  "Appends to `re-suggest-cmd-string' the character that
  `this-original-command' maps to in `re-suggest-cmd-char-alist',
  or \" \" if no match exists."
  (re-suggest-verify-cmd-string)
  (setq re-suggest-cmd-string
        (concat (subseq re-suggest-cmd-string 1)
                (or (cdr (assoc this-original-command re-suggest-cmd-char-alist))
                    " "))))

(defun re-suggest-detect-match ()
  "Attempts to match `re-suggest-cmd-string' against all the
regexps in `re-suggest-regexp-cmd-seq-alist'. Returns the
corresponging message if a match is found, or nil otherwise."
  (catch 'result
    (let (case-fold-search)
      (mapc (lambda (seq) (and (string-match (car seq) re-suggest-cmd-string)
                          (throw 'result (cdr seq))))
            re-suggest-regexp-cmd-seq-alist)
      nil)))

(defun re-suggest-hook ()
  "Main re-suggest Hook that gets added to `post-command-hook'."
  (re-suggest-record-cmd)
  (let ((msg (re-suggest-detect-match)))
    (when (and msg (re-suggest-check-time))
      (message msg)
      (ding)
      (setq re-suggest-cmd-string (re-suggest-make-empty-cmd-string)))))

;;;###autoload
(define-minor-mode re-suggest-mode
  "Toggle re-suggest minor mode.

If ARG is null, toggle re-suggest.
If ARG is a number greater than zero, turn on re-suggest.
Otherwise, turn off re-suggest."
  :init-value nil
  (cond
   (noninteractive
    (remove-hook 'post-command-hook 're-suggest-hook)
    (setq re-suggest-mode nil))
   (re-suggest-mode
    (add-hook 'post-command-hook 're-suggest-hook)
    (setq re-sugguest-mode t))
   (t
    (remove-hook 'post-command-hook 're-suggest-hook)
    (setq re-suggest-mode nil))))

(provide 're-suggest)
