;;; eltex.el --- Write LaTeX in Emacs Lisp

;; Copyright (C) 2015 Oleh Krehel

;; Author: Oleh Krehel <ohwoeowho@gmail.com>
;; URL: https://github.com/abo-abo/eltex
;; Version: 0.1.0
;; Keywords: latex elisp

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; With a few functions and macros, you can write Emacs Lisp code that
;; evaluates to a string of LaTeX code.
;;
;; This file provides a major mode, derived from `emacs-lisp-mode'
;; that improves the highlighting of these functions and macros.
;;
;; It also provides a function `eltex-compile', which will export the
;; current ElTeX buffer to a LaTeX file specified in `eltex-filename'.
;; See paperdemo.elt for an example of the general syntax.

;;; Code:

(require 'eltex-base)

(defvar eltex-filename)

(defun eltex-compile (arg)
  "Generate a LaTeX file from the current .elt file.
When ARG is nil, write it to `eltex-filename'.
Otherwise, write it to a temporary *Generated LaTeX* buffer."
  (interactive "P")
  (save-buffer)
  (let* ((source (format
                  "(progn %s )"
                  (buffer-substring-no-properties
                   (point-min)
                   (point-max))))
         (tex-source (read source))
         (dc (assoc 'documentclass tex-source))

         (tex-name-statement (cl-find-if
                              (lambda (x)
                                (and (listp x)
                                     (eq (car x) 'setq)
                                     (eq (cadr x) 'eltex-filename)))
                              tex-source)))
    (eval tex-name-statement)
    (assq-delete-all 'documentclass tex-source)
    (if (or arg (null tex-name-statement))
        (switch-to-buffer-other-window
         (get-buffer-create "*Generated LaTeX*"))
      (find-file-other-window eltex-filename))
    (delete-region (point-min) (point-max))
    (latex-mode)
    (eval tex-source)
    (insert (eval dc))
    (indent-region (point-min) (point-max))
    (when (and tex-name-statement (not arg)) (save-buffer))))

(define-derived-mode eltex-mode emacs-lisp-mode "el-TeX"
    "eltex mode is a major mode for generating TeX from emacs lisp."
    (font-lock-add-keywords
     nil
     `(("\""
        (0 font-lock-string-face t)
        (0 (prog1
               (compose-region
                (match-beginning 0)
                (match-end 0)
                "'")
             nil)))
       (,(format
          "(\\(%s\\)\\>"
          (regexp-opt '("documentclass" "document" "usepackage" "bibliography" "with"
                        "section" "align" "equation" "cite" "table" "tabular"
                        "defsection" "defun" "cond" "if" "txt" "let"
                        "let*" "defmath" "eltex-add-math" "row" "em" "quotation"
                        "itemize" "enumerate" "description" "center")))
         (1 font-lock-keyword-face t))
       ("(\\(\\$\\)" (1 font-lock-keyword-face)))))

(make-face 'eltex-text-face)
(set-face-background 'eltex-text-face "white")
(set-face-foreground 'eltex-text-face "SaddleBrown")

(provide 'eltex)

;;; eltex.el ends here
