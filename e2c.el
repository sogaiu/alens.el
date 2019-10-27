;;; e2c.el --- elisp data to clojure data -*- lexical-binding: t -*-

;; Author: sogaiu
;; Version: 20191026
;; Package-Requires: ((emacs "26.3"))
;; Keywords: emacs-lisp, clojure

;; This file is not part of GNU Emacs.

;;; Commentary:

;; From Emacs Lisp data, produce reasonable Clojure printed representation

;; Originally based on a portion of clomacs by Kostafey <kostafey@gmail.com>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; XXX: consider whether and how to handle plists
;; XXX: other data?

;;; Code:

(require 'cl-lib)

(declare e2c-pr-str)

(defun e2c-alist-p (e-obj)
  "Return non-nil if E-OBJ is an alist, else nil."
  ;; XXX: weak?
  (and (listp e-obj)
       (car e-obj)
       (listp (car e-obj))
       (not (listp (cdr (car e-obj))))))

(defun e2c-wrap-map-content (inner-str)
  "Wrap INNER-STR in curly braces."
  (let ((i-len (length inner-str)))
    (if (= 0 i-len)
        "{}"
      (concat "{"
              ;; extra work to remove one space...
              (substring inner-str 0 (1- i-len))
              "}"))))

(defun e2c-alist->map (alist)
  "Return printed representation of Clojure map for ALIST."
  (let* ((inner (cl-reduce
                 (lambda (acc pair)
                   (concat acc
                           (e2c-pr-str (car pair)) " "
                           (e2c-pr-str (cdr pair)) " "))
                 alist
                 :initial-value "")))
    (e2c-wrap-map-content inner)))

(defun e2c-hash-table->map (hash-table)
  "Return printed representation of Clojure map for HASH-TABLE."
  (let ((inner ""))
    ;; collect content of hash-table
    (maphash
     (lambda (key val)
       (setq inner
             (concat inner
                     (e2c-pr-str key) " "
                     (e2c-pr-str val) " ")))
     hash-table)
    ;; construct string for content
    (e2c-wrap-map-content inner)))

(defun e2c-str->str (str)
  "Return printed representation of Clojure string for STR."
  (format "%S"
          (progn
            (set-text-properties 0 (length str) nil str) ; strip
            str)))

(defun e2c-pr-str (e-obj)
  "Return Clojure printed representation for Emacs Lisp E-OBJ."
  (cond
   ((numberp e-obj)
    (number-to-string e-obj))
   ((stringp e-obj)
    (e2c-str->str e-obj))
   ((booleanp e-obj)
    (if e-obj
        "true"
      "false"))
   ((e2c-alist-p e-obj)
    (e2c-alist->map e-obj))
   ((hash-table-p e-obj)
    (e2c-hash-table->map e-obj))
   ((and (listp e-obj)
         (equal (car e-obj) 'quote))
    (concat "'"
            (symbol-name (cadr e-obj)))) ; XXX: iffy?
   ((listp e-obj)
    (concat "("
            (mapconcat 'e2c-pr-str e-obj " ")
            ")"))
   ((symbolp e-obj)
    (symbol-name e-obj))
   (t
    (replace-regexp-in-string
     "\\\\." "." (format "%S" e-obj))))) ; leading quote was problematic

(provide 'e2c)

;;; e2c.el ends here
