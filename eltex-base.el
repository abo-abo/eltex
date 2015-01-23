;;; eltex-base.el --- basic implementation functions of ElTeX

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
(require 'cl-lib)

(defun eltex--usepackage (name &rest options)
  "Require one package NAME with OPTIONS."
  (format "\\usepackage%s{%s}"
          (mapconcat (lambda (op)
                       (format "[%s]" op))
                     options
                     "")
          name))

(defun eltex-join (forms)
  "Concatenate list of strings FORMS, taking care of newlines."
  (if forms
      (cl-reduce
       (lambda (a b)
         (concat
          a
          (if (and (> (length b) 0)
                   (eq (aref b 0) ?\n)
                   (eq (aref a (1- (length a))) ?\n))
              (substring b 1)
            b)))
       forms)
    ""))

(defun eltex-interleave (lst val)
  "Return a list created from LST by interleaving VAL."
  (cdr (cl-mapcan (lambda (x) (list val x)) lst)))

(defun eltex-matrix-col-widths (mat)
  "Return list of maximal widths of columns of MAT."
  (let (col-width out)
    (while (> (setq col-width (cl-reduce #'max (mapcar (lambda (x) (length (car x))) mat)))
              0)
      (push col-width out)
      (setq mat (mapcar #'cdr mat)))
    (nreverse out)))

(defun eltex-matrix-widen (mat)
  "Format the rows of matrix MAT."
  (when mat
    (let ((fstrs (mapcar (lambda (w) (format "%%-%ds" w))
                         (eltex-matrix-col-widths mat))))
      ;; don't widen last column
      (setcar (last fstrs) "%s")
      (mapcar
       (lambda (x)
         (cl-mapcar (lambda (f s)
                      (format f s))
                    fstrs
                    x))
       mat))))

(defun eltex-remove-text-property (str prop)
  "Remove from STR the property PROP."
  (remove-text-properties 0 (length str) (list prop) str)
  str)

;;* Math functions
(defmacro defmath (name str &rest props)
  `(progn
     (setq ,name (eltex-add-math ,str))
     (setplist ',name (list ,@props))
     (put ',name :math t)
     ,name))

(defun eltex-add-math (&rest strs)
  "Concatenate STRS and add :math property to them."
  (propertize (apply #'concat strs) :math t))

(defun eltex-add-emphasis (&rest strs)
  "Concatenate STRS and add :em property to them."
  (propertize (apply #'concat strs) :em t))

(defun eltex-remove-math (str)
  "Remove :math property from STR."
  (eltex-remove-text-property str :math))

(defun eltex-concat-tex (s1 s2)
  (format "%s%s%s"
          s1
          (if (or (memq (aref s2 0) '(?. ?\,)) (string= s1 ""))
              ""
            " ")
          s2))

(defun eltex-unalias-property (prop str template)
  "Unalias PROP from STR with TEMPLATE."
  (let ((j t)
        out
        (i 0))
    (while j
      (setq j (next-single-property-change i prop str))
      (let ((s (eltex-remove-text-property (substring str i j) prop)))
        (push (if (get-char-property i prop str)
                  (format template s)
                s)
              out))
      (setq i j))
    (cl-reduce #'eltex-concat-tex (nreverse out))))

(defun eltex-unmath (str)
  "Unalias math in STR."
  (eltex-unalias-property :math str "$%s$"))

(defun eltex-un-em (str)
  "Unalias \\em in STR."
  (eltex-unalias-property :em str "{\\em %s}"))

;;* Globals
(defvar eltex-meta (make-hash-table :test 'equal)
  "Meta properties for ElTeX document.")

(defun eltex-set-property (x key value)
  "Store in `eltex-meta' X the KEY VALUE pair."
  (let ((y (gethash x eltex-meta)))
    (unless y
      (setq y (puthash x (make-hash-table :test 'equal) eltex-meta)))
    (puthash key value y)))

(defun eltex-get-property (x key)
  "Get `eltex-meta' X value from KEY."
  (let ((y (gethash x eltex-meta)))
    (when y
      (gethash key y))))

;;* Lexical structures
(defun eltex-reinterpret (&rest forms)
  "A helper of (`eltex-with' FORMS)."
  (let ((str (eltex-join forms)))
    (while (string-match "@\\([a-zA-Z-0-9]+\\)" str)
      (let* ((m (match-string 1 str))
             (var (intern-soft m)))
        (if var
            (setq str (replace-match (eval var) t t str))
          (error "Bad @-expression: %s" m))))
    str))

(defmacro eltex-with (bindings &rest forms)
  `(let* ,bindings
     (eltex-reinterpret ,@forms)))

;;* LaTeX primitives helpers
(defmacro eltex-tabular-row (&rest elts)
  "A helper to unalias ELTS for LaTeX tabular."
  (let ((nelts (mapcar
                (lambda (x)
                  (eltex-unmath
                   (if (and (listp x) (not (functionp (car x))))
                       (mapconcat #'eval x "")
                     (eval x))))
                elts)))
    `(list ,@nelts)))

(defmacro eltex-itemize-item (&rest elts)
  "A helper to unalias ELTS for LaTeX itemize."
  (let ((nelts (mapcar
                (lambda (x)
                  (eltex-unmath
                   (if (and (listp x)
                            (not (or (functionp (car x))
                                     (macrop (car x)))))
                       (mapconcat #'eval x "")
                     (eval x))))
                elts)))
    `(list ,@nelts)))

(provide 'eltex-base)

;;; eltex-base.el ends here
