 ;;; pandoc-template-mode.el --- Pandoc-Template major mode

;; Copyright (C) 2017

;; Author: Václav Haisman
;; Keywords: extensions

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

 ;;; Commentary:

;;

 ;;; Code:

(defvar pandoc-template-font-lock-keywords
  '(("\\(\\$--.*\\)$"
      (1 font-lock-comment-face))
    ("\\(\\$\\)\\(if\\|for\\)(\\([^)]+\\))\\(\\$\\)"
      (1 font-lock-preprocessor-face)
      (2 font-lock-keyword-face)
      (3 font-lock-variable-name-face)
      (4 font-lock-preprocessor-face))
     ("\\(\\$\\)\\(endif\\|endfor\\|else\\)\\(\\$\\)"
      (1 font-lock-preprocessor-face)
      (2 font-lock-keyword-face)
      (3 font-lock-preprocessor-face))
     ("\\(\\$\\)\\(sep\\)\\(\\$\\)"
      (1 font-lock-preprocessor-face)
      (2 font-lock-builtin-face)
      (3 font-lock-preprocessor-face))
     ("\\(\\$\\)\\([^$]+\\)\\(\\$\\)"
      (1 font-lock-preprocessor-face)
      (2 font-lock-variable-name-face)
      (3 font-lock-preprocessor-face))
     )
  "Keyword highlighting specification for `pandoc-template-mode'.")

 ;;;###autoload
(define-derived-mode pandoc-template-mode fundamental-mode "Pandoc-Template"
  "A major mode for editing Pandoc-Template files."
  :syntax-table text-mode-syntax-table
  (setq-local font-lock-defaults
              '(pandoc-template-font-lock-keywords))
  (setq-local comment-start "$--")
  (setq-local comment-start-skip "\\$--[ \t]*")
  (setq-local comment-end "")
  (setq-local comment-end-skip "[ \t]*$"))

(provide 'pandoc-template-mode)
 ;;; pandoc-template.el ends here
