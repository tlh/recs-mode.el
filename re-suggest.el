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
;;    `recs-cmd-char-alist' to an alist of command to
;;    character-string mappings like so:
;;
;;    (setq recs-cmd-char-alist
;;      '((newline         . "l")
;;        (previous-line   . "p")
;;        (next-line       . "n")))
;;
;;  - You'll also need to set the mapping of command sequence regexps
;;    to suggestion messages. To do so, set the value of
;;    `recs-regexp-cmd-seq-alist' to an alist of
;;    command-sequence-regexp to suggestion-message mappings like so:
;;
;;    (defvar recs-regexp-cmd-seq-alist
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
;;    (setq recs-cmd-string-length foo)
;;
;;  - You can set the minimum number of seconds between suggestions by
;;    setting `recs-suggestion-interval':
;;
;;    (setq recs-suggestion-interval 60)
;;
;;    If set to nil, suggestions will be made for every match.
;;

;; TODO:
;;
;;  - More default suggestions
;;
;;  - Mode specific command regexp matching?
;;

;;; Code:

(eval-when-compile
  (require 'cl))

(defgroup recs nil
  "Regexp-based command suggestion minor mode."
  :group 'extensions
  :group 'convenience
  :version "1.0")

;; Customizable variables

(defcustom recs-null-cmd-char "_"
  "Placeholder string used to represent `recs-cmd-string'
  commands not defined in `recs-cmd-char-alist'. Value must be a
  string of length 1, and can not be used as a value in
  `recs-cmd-char-alist'."
  :type 'string
  :group 'recs)

(defcustom recs-suggestion-interval nil
  "Defines the minimum number of seconds between suggestions. If
  nil, no time checking is performed."
  :type 'boolean
  :group 'recs)

(defcustom recs-ding-on-suggestion t
  "Defines whether to call `ding' when a suggestion is made."
  :type 'boolean
  :group 'recs)

(defcustom recs-pop-to-buffer nil
  "Defines whether to pop to a `q'-dismissable suggestion buffer,
rather than sending the suggestion to the echo area."
  :type 'boolean
  :group 'recs)

(defcustom recs-cmd-char-alist
  ;; These commands won't be in any order that makes sense. I assigned
  ;; characters to them mnemonically at the beginning, before running
  ;; out of good ones, then alphabetized based on those characters to
  ;; better see which characters were available.
  '((move-beginning-of-line               . "a")
    (backward-char                        . "b")
    (backward-word                        . "B")
    (backward-list                        . "C")
    (set-mark-command                     . "c")
    (delete-window                        . "d")
    (delete-char                          . "D")
    (backward-delete-char-untabify        . "E")
    (move-end-of-line                     . "e")
    (forward-char                         . "f")
    (forward-word                         . "F")
    (forward-list                         . "G")
    (kill-ring-save                       . "g")
    (indent-for-tab-command               . "i")
    (kill-line                            . "k")
    (kill-region                          . "K")
    (newline                              . "l")
    (mark-paragraph                       . "M")
    (next-line                            . "n")
    (other-window                         . "o")
    (previous-line                        . "p")
    (beginning-of-buffer                  . "r")
    (eval-last-sexp                       . "s")
    (kill-word                            . "t")
    (backward-kill-word                   . "T")
    (scroll-up                            . "v")
    (scroll-down                          . "V")
    (end-of-buffer                        . "w")
    (forward-paragraph                    . "x")
    (yank                                 . "y")
    (backward-paragraph                   . "z")
    )
  "An alist mapping commands to character strings. It's used to
convert sequences of commands into strings. The character string
defined in `recs-null-cmd-char' is reserved for commands
not present in this list, and should not be used. Modify this
list to suit your needs."
  :type 'alist
  :group 'recs)

(defcustom recs-regexp-cmd-seq-alist
  '(("lpe"                                . "You should use `open-line' to do that.")
    ("xs"                                 . "You should use `eval-defun' to do that.")
    ("li"                                 . "You should use `newline-and-indent' to do that.")
    ("kkny"                               . "You should use `transpose-lines' to do that.")
    ("ov"                                 . "You should use `scroll-other-window' to do that.")
    ("FFT"                                . "You should use `kill-word' to do that.")
    ("BBt"                                . "You should use `backward-kill-word' to do that.")
    ("zcx\\|xcz"                          . "You should use `mark-paragraph' to do that.")
    ("rcw\\|wcr"                          . "You should use `mark-whole-buffer' to do that.")
    ("MK[z\|x]+y"                         . "You should use `transpose-paragraphs' to do that.")
    ("c[G\|C]+K[G\|C]+l*y"                . "You should use `transpose-sexps' to do that.")
    ("c[F\|B]K[F\|B]+y"                   . "You should use `transpose-words' to do that.")
    ;; These can get a little annoying:
    ;; ("D\\{15\\}"                          . "You should use something like `kill-word' to do that.")
    ;; ("E\\{15\\}"                          . "You should use something like `backward-kill-word' to do that.")
    ;; ("n\\{20\\}"                          . "You should use something like `forward-paragraph'.")
    ;; ("p\\{20\\}"                          . "You should use something like `backward-paragraph'.")
    ;; ("f\\{20\\}"                          . "You should use something like `forward-word'.")
    ;; ("b\\{20\\}"                          . "You should use something like `backward-word'.")
    )
  "An alist mapping command sequence regexps to suggestion
  messages. Substrings quoted `like-this' are considered to be
  command names, and an attempt is made to determine and print
  their keybindings. Modify this list to suit your needs."
  :type 'alist
  :group 'recs)

;; Nonconfigurable variables

(defvar recs-cmd-string nil
  "A ring-like string composed of characters that map to commands
  in `recs-cmd-char-alist'. When a command is entered, its
  command-character (or `recs-null-cmd-char' if the command has
  no mapping in `recs-cmd-char-alist') is appended to the end of
  the string, and the oldest command-character is removed from
  the beginning. Its length will always be
  `recs-cmd-string-length'.")

(defvar recs-cmd-string-length 100
  "Length of `recs-cmd-string'. Increase this number in order to
  be able to detect longer patterns of commands.")

(defvar recs-suggestion-buffer-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") (lambda () (interactive) (throw 'exit nil)))
    map)
  "Keymap for commands in the suggestion buffer.")

(defvar recs-last-suggestion-time nil
  "System seconds at which the last suggestion occured.")

(defun recs-current-time ()
  "Returns the current system time in seconds."
  (let ((time (current-time)))
    (+ (* (car time) (expt 2 16))
       (cadr time)
       (/ (caddr time) 1000000.0))))

(defun recs-check-time ()
  "Returns t if `recs-suggestion-interval' is nil, if
`recs-last-suggestion-time' is nil or if the sum of the
previous two has been superceded, nil otherwise."
  (or (not recs-suggestion-interval)
      (not recs-last-suggestion-time)
      (>=  (recs-current-time)
           (+ recs-last-suggestion-time
              recs-suggestion-interval))))

(defun recs-record-time ()
  "Sets `recs-last-suggestion-time' with the current time
in seconds."
  (setq recs-last-suggestion-time (recs-current-time)))

(defun recs-reset-cmd-string ()
  "Makes and empty cmd-string of length
`recs-cmd-string-length'."
  (setq recs-cmd-string
        (make-string recs-cmd-string-length ? )))

(defun recs-verify-cmd-string ()
  "Verifies that `recs-cmd-string' exists, is a string, and
is of the length `recs-cmd-string-length'."
  (unless (and recs-cmd-string
               (stringp recs-cmd-string)
               (= (length recs-cmd-string)
                  recs-cmd-string-length))
    (recs-reset-cmd-string))
  recs-cmd-string)

(defun recs-record-cmd ()
  "Appends to `recs-cmd-string' the character that
  `this-original-command' maps to in `recs-cmd-char-alist',
  or \" \" if no match exists."
  (recs-verify-cmd-string)
  (setq recs-cmd-string
        (concat (subseq recs-cmd-string 1)
                (or (cdr (assoc this-original-command recs-cmd-char-alist))
                    recs-null-cmd-char))))

(defun recs-detect-match ()
  "Attempts to match `recs-cmd-string' against all the
regexps in `recs-regexp-cmd-seq-alist'. Returns the
corresponging message if a match is found, nil otherwise."
  (catch 'result
    (let (case-fold-search)
      (dolist (pattern recs-regexp-cmd-seq-alist nil)
        (and (string-match (car pattern) recs-cmd-string)
             (throw 'result (cons (match-string 0 recs-cmd-string)
                                  pattern)))))))

(defun recs-princ-suggestion (match)
  "Princ the suggestion."
  (flet ((mprinc (&rest args) (mapc 'princ args)))
    (let ((msg (cddr match)) (pos 0))
      (princ "You entered the command sequence:\n\n[")
      (dolist (s (split-string (car match) "" t))
        (dolist (elt recs-cmd-char-alist)
          (when (string= s (cdr elt))
            (mprinc "`" (car elt) "', "))))
      (mprinc "]\n\n" msg "\n\n")
      (while (string-match "`\\(.*?\\)'" msg pos)
        (setq pos (match-end 1))
        (let* ((cmd (intern (match-string 1 msg)))
               (keys (where-is-internal (or (command-remapping cmd) cmd))))
          (cond ((not (commandp cmd))
                 (mprinc cmd "is actually not a command."))
                (keys
                 (mprinc "`" cmd "' is bound to: "
                         (mapconcat 'key-description keys ", ")))
                (t (mprinc cmd " has no keybindings."))))))))

(defun recs-suggest (match)
  "Displays suggestion to user in a separate buffer if
  `recs-pop-to-buffer' is non-nil, otherwise in the echo area."
  (if recs-pop-to-buffer
      (with-help-window "*recs*" (recs-princ-suggestion match))
    (message (with-output-to-string (recs-princ-suggestion match)))))

(defun recs-hook ()
  "Hook function to add to `post-command-hook'."
  (when (recs-check-time)
    (recs-record-cmd)
    (let ((match (recs-detect-match)))
      (when match
        (recs-reset-cmd-string)
        (recs-record-time)
        (when recs-ding-on-suggestion (ding))
        (recs-suggest match)))))

(defun recs-enable (enable)
  "Enables `re-suggest-mode' when ENABLE is t, disables
otherwise."
  (cond (enable (add-hook 'post-command-hook 'recs-hook)
                (recs-reset-cmd-string)
                (setq re-suggest-mode t))
        (t      (remove-hook 'post-command-hook 'recs-hook)
                (setq re-suggest-mode nil))))

;;;###autoload
(define-minor-mode re-suggest-mode
  "Toggle re-suggest minor mode.

If ARG is null, toggle re-suggest.
If ARG is a number greater than zero, turn on re-suggest.
Otherwise, turn off re-suggest."
  :lighter     " recs"
  :init-value  nil
  :global      t
  (cond (noninteractive   (recs-enable nil))
        (re-suggest-mode  (recs-enable t))
        (t                (recs-enable nil))))

(provide 're-suggest)
