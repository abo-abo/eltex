;;; eltex-macros.el --- some LaTeX-like macros for ElTeX

;; Copyright (C) 2015 Oleh Krehel

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
;; The code here is akin to a LaTeX user defining her own macros for a
;; specific document.

;;; Code:

(require 'eltex-latex)

;;* Spaces and Sets
(defun Real (n)
  (mformat "\\mathbb{R}^%s" n))

(defun Integer (n)
  (mformat "\\mathbb{Z}^%s" n))

(defun in-range (from &optional to)
  (if to
      (format "\\in\\{%s,\\ldots,%s\\}" from to)
    (format "\\in\\{1,\\ldots,%s\\}" from)))

(defun unit-vector (n)
  (mformat "\\vec{e}_%s" n))

;;* Subscripts and Superscripts
(defun partial (x)
  (mformat "\\partial %s" x))

(defun sub (x y)
  (if (string-match "_" x)
      (error "Double subscript")
    (let* ((base x)
           (rest (format "_{%s}" y))
           (z (concat base rest)))
      (eltex-set-property z "base" base)
      (eltex-set-property z "rest" rest)
      ($ z))))

(defun closure (x)
  (format "\\overline{%s}%s"
          (eltex-get-property x "base")
          (eltex-get-property x "rest")))

(defun sum (i to &optional from)
  (unless from (setq from "1"))
  (format "\\sum_{%s=%s}^{%s}" i from to))

(defun seq (&rest str)
  (format "\\{%s\\}" (apply #'concat str)))

;;* Math

;; TODO: maybe make a special case for when one of OBJECTS isn't in
;; math-mode
(defun mformat (fstr &rest objects)
  "Return same as `format', except with :math t property."
  (apply #'format (eltex-add-math fstr) objects))
;;* Whitespace
(defconst \n "\n"
  "A symbol that represents a  newline.")

(defconst \\ "\n\n"
  "A symbol that represents a double newline.")

(provide 'eltex-macros)

;;; eltex-macros.el ends here
