;;; eltex-latex.el --- LaTeX primitives definitions for ElTeX

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

;;; Code:

(require 'eltex-base)

(defun documentclass (class &rest forms)
  "Define a LaTeX document with CLASS.
FORMS are a list of strings that form the document.

Implements:

\\documentclass{CLASS}
FORMS"
  (format
   "\\documentclass%s
%s
" (if (stringp class)
      (format "{%s}" class)
    (format "[%s]{%s}"
            (mapconcat #'identity (cdr class) ", ")
            (car class)))
   (mapconcat #'identity forms "\n")))

(defun document (&rest forms)
  "Define a LaTeX document body with FORMS inside.

Implements:

\\begin{document}
FORMS
\\end{document}"
  (format
   "\\begin{document}
%s
\\end{document}"
   (mapconcat #'identity forms "\n")))

(defun usepackage (&rest packages)
  "Generate LaTeX usepackage commands for PACKAGES.
Eacs package in PACKAGES is either a string of a list of strings.
In the latter case, the first element of the list is the package name,
while the rest are the package options."
  (mapconcat (lambda (x)
               (apply #'eltex--usepackage
                      (if (listp x) x (list x))))
             packages
             "\n"))

(defun bibliography (bib)
  "Generate \\addbibresource{BIB}."
  (format "\\addbibresource{%s.bib}" bib))

(defun table (&optional placement &rest forms)
  "Define a LaTeX table.

Implements:

\\begin{table}[PLACEMENT]
FORMS
\\end{table}"
  (let ((option (if placement (format "[%s]" placement) "")))
    (eltex-join
     (list
      (format "\n\\begin{table}%s\n" option)
      (eltex-join forms)
      "\n\\end{table}\n"))))

(defmacro tabular (fmt &rest rows)
  "Define a LaTeX tabular environment.

Implements:

\\begin{tabular}[FMT]
ROWS
\\end{tabular}"
  (eltex-join
   (list
    (format "\n\\begin{tabular}{%s}\n" fmt)
    (mapconcat (lambda (x)
                 (concat (apply #'concat (eltex-interleave x " & ")) "\\\\"))
               (eltex-matrix-widen
                (mapcar
                 (lambda (x)
                   (if (memq (car x) '(with let let* with*))
                       (eval x)
                     (eval (cons 'eltex-tabular-row x))))
                 rows))
               "\n")

    "\n\\end{tabular}")))

(defun label (x)
  "LaTeX label command.

Implements: \\label{X}."
  (format "\\label{%s}\n" x))

(defun align (&rest forms)
  "LaTeX align command.

Implements:
\\begin{align}
FORMS
\\end{align}"
  (eltex-remove-math
   (format
    "\n\\begin{align}
%s
\\end{align}"
    (mapconcat #'identity forms ""))))

(defun equation (&rest forms)
  "LaTeX equation command.

Implements:
\\begin{equation}
FORMS
\\end{equation}"
  (eltex-remove-math
   (format
    "\n\\begin{equation}
%s
\\end{equation}"
    (mapconcat #'identity forms ""))))

(defun cite (x)
  "LateX cite command.

Implements: \\cite{X}."
  (format "\\cite{%s}" x))

(defvar eltex-section-depth -1
  "Variable to store the current `section' depth.")

(defmacro section (name &rest forms)
  "LaTeX section with NAME consisting of FORMS."
  (let ((own))
    `(let ((eltex-section-depth (1+ eltex-section-depth)))
       (concat (format "\n\\%ssection{%s}\n"
                       (mapconcat #'identity (make-list eltex-section-depth "sub") "")
                       ,name)
               (eltex-un-em
                (eltex-unmath
                  (mapconcat #'identity
                             (list ,@forms)
                             "")))))))

(defmacro defsection (name &rest forms)
  "LaTeX section with NAME consisting of FORMS."
  `(defun ,name ()
     (section ,@forms)))

(defalias 'row 'eltex-tabular-row)
(defalias '$ 'eltex-add-math)
(defalias 'em 'eltex-add-emphasis)
(defalias 'with 'eltex-with)

(defun maketitle (&rest plist)
  "Generate LaTeX \\maketitle command.
PLIST should provide at least :title and :author keys."
  (let ((title (plist-get plist :title))
        (author (plist-get plist :author)))
    (format "\\title{%s}\n\\author{%s}\n\\maketitle"
            title author)))

(defun quotation (&rest forms)
  "Define a LaTeX quotation with FORMS inside.

Implements:

\\begin{quotation}
FORMS
\\end{quotation}"
  (format
   "\n\\begin{quotation}
%s
\\end{quotation}"
   (mapconcat #'identity forms "\n")))

(defun center (&rest forms)
  "Define a LaTeX center with FORMS inside.

Implements:

\\begin{center}
FORMS
\\end{center}"
  (format
   "\n\\begin{center}
%s
\\end{center}"
   (mapconcat #'identity forms "\n")))

(defmacro tabular (fmt &rest rows)
  "Define a LaTeX tabular environment.

Implements:

\\begin{tabular}[FMT]
ROWS
\\end{tabular}"
  (eltex-join
   (list
    (format "\n\\begin{tabular}{%s}\n" fmt)
    (mapconcat (lambda (x)
                 (concat (apply #'concat (eltex-interleave x " & ")) "\\\\"))
               (eltex-matrix-widen
                (mapcar
                 (lambda (x)
                   (if (memq (car x) '(with let let* with*))
                       (eval x)
                     (eval (cons 'eltex-tabular-row x))))
                 rows))
               "\n")

    "\n\\end{tabular}")))

(defmacro itemize (&rest items)
  "Define a LaTeX itemize from ITEMS.

ITEMS is a list of lists of strings.

Implements:

\\begin{itemize}
\\item item1
\\item item2
\\end{itemize}"
  (eltex-join
   (list
    "\n\\begin{itemize}\n"
    (mapconcat (lambda (x)
                 (concat "\\item " (apply #'concat x) "\n"))
               (eltex-matrix-widen
                (mapcar
                 (lambda (x)
                   (if (memq (car x) '(with let let* with*))
                       (eval x)
                     (eval (cons 'eltex-itemize-item x))))
                 items))
               "\n")

    "\n\\end{itemize}")))

(defmacro enumerate (&rest items)
  "Define a LaTeX enumerate from ITEMS.

ITEMS is a list of lists of strings.

Implements:

\\begin{enumerate}
\\item item1
\\item item2
\\end{enumerate}"
  (eltex-join
   (list
    "\n\\begin{enumerate}\n"
    (mapconcat (lambda (x)
                 (concat "\\item " (apply #'concat x) "\n"))
               (eltex-matrix-widen
                (mapcar
                 (lambda (x)
                   (if (memq (car x) '(with let let* with*))
                       (eval x)
                     (eval (cons 'eltex-itemize-item x))))
                 items))
               "\n")

    "\n\\end{enumerate}")))

(defmacro description (&rest items)
  "Define a LaTeX description from ITEMS.

ITEMS is a list of lists of strings.

Implements:

\\begin{description}
\\item item1
\\item item2
\\end{description}"
  (eltex-join
   (list
    "\n\\begin{description}\n"
    (mapconcat (lambda (x)
                 (format "\\item[%s] %s"
                         (car x)
                         (apply #'concat (cdr x)) "\n"))
               (eltex-matrix-widen
                (mapcar
                 (lambda (x)
                   (if (memq (car x) '(with let let* with*))
                       (eval x)
                     (eval (cons 'eltex-itemize-item x))))
                 items))
               "\n")

    "\n\\end{description}")))

(provide 'eltex-latex)

;;; eltex-latex.el ends here
