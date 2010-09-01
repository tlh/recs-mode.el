;;; recs-patterns.el --- recs-mode pattern definitions

;; Copyright (C) 2010 tlh <thunkout@gmail.com>

;; File:      recs-patterns.el
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
;;  This is an example recs-mode pattern definitions file.  To
;;  customize command suggestions, you should copy this file somewhere
;;  like ~/.emacs.d and modify it there.  See the documentation in
;;  recs-mode.el for more information.
;;

;;; Code:

(recs-add-pattern
 (list "newline previous-line move-end-of-line"
       "You should use `open-line' to do that."
       'open-line))

(recs-add-pattern
 (list "newline indent-for-tab-command"
       "You should use `newline-and-indent' to do that."
       'newline-and-indent))

(recs-add-pattern
 (list "forward-paragraph eval-last-sexp"
       "You should use `eval-defun' to do that."
       'eval-defun))

(recs-add-pattern
 (list "other-window scroll-up"
       "You should use `scroll-other-window' to do that."
       'scroll-other-window))

(recs-add-pattern
 (list "other-window scroll-down"
       "You should use `scroll-other-window-down' to do that."
       'scroll-other-window-down))

(recs-add-pattern
 (list "move-beginning-of-line indent-for-tab-command"
       "You should use `back-to-indentation' to do that."
       'back-to-indentation))

(recs-add-pattern
 (list "other-window \\(find-file\\|ido-find-file\\)"
       "You should use `find-file-other-window' to do that."
       'find-file-other-window))

(recs-add-pattern
 (list "kill-line \\(kill-line \\|delete-char \\)\\(move-end-of-line\
 newline \\|next-line \\(open-line \\)*\\)yank"
       "You should use `transpose-lines' to do that."
       'transpose-lines))

(recs-add-pattern
 (list "\\(beginning-of-defun set-mark-command \\(end-of-defun\\|\
forward-sexp\\)\\|end-of-defun set-mark-command \\(beginning-of-defun\
\\|backward-sexp\\)\\)"
       "You should use `mark-defun' to do that."
       'mark-defun))

(recs-add-pattern
 (list "\\(backward-paragraph set-mark-command forward-paragraph\\|\
forward-paragraph set-mark-command backward-paragraph\\)"
       "You should use `mark-paragraph' to do that."
       'mark-paragraph))

(recs-add-pattern
 (list "\\(beginning-of-buffer set-mark-command end-of-buffer\\|\
end-of-buffer set-mark-command beginning-of-buffer\\)"
       "You should use `mark-whole-buffer' to do that."
       'mark-whole-buffer))

(recs-add-pattern
 (list "mark-paragraph kill-region \\(backward-paragraph \\|\
forward-paragraph \\)+yank"
       "You should use `transpose-paragraphs' to do that."
       'transpose-paragraphs))

(recs-add-pattern
 (list "set-mark-command \\(forward-list \\|backward-list \\)+kill-region \
\\(forward-list \\|backward-list \\)+\\(newline \\)*yank"
       "You should use `transpose-sexps' to do that."
       'transpose-sexps))

(recs-add-pattern
 (list "set-mark-command \\(forward-word \\|backward-word \\)kill-region \
\\(forward-word \\|backward-word \\)+yank"
       "You should use `transpose-words' to do that."
       'transpose-words))

;; recs-patterns.el ends here
