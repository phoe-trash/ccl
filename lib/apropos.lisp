;;;-*-Mode: LISP; Package: CCL -*-
;;;
;;; Copyright 1994-2009 Clozure Associates
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;     http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

(in-package "CCL")

(eval-when (:execute :compile-toplevel)
   (require :level-2))

;;; APROPOS-LIST

(defun apropos-list (string &optional package)
  "Like APROPOS, except that it returns a list of the symbols found instead
  of describing them."
  (require-type string '(or symbol string character))
  (require-type package '(or null package symbol string character))
  (let ((result '()))
    (flet ((maybe-collect (symbol)
             (when (search (string string) (symbol-name symbol) :test #'char-equal)
               (pushnew symbol result))))
      (if package
        (do-symbols (symbol package) (maybe-collect symbol))
        (do-all-symbols (symbol) (maybe-collect symbol))))
    (sort result #'string-lessp)))

;;; DEFINE-APROPOS-NAMESPACE

(defvar *apropos-namespaces* (make-hash-table)
  "The table of all namespaces known to APROPOS. Each key is a keyword that is meant
for uniquely representing the namespace and each value is a list of a user-friendly
name of the namespace that is printed by APROPOS, and a predicate that accepts a
symbol and returns true if the symbol is bound to that definition.
\
Programmers may add new forms to this table to have APROPOS recognize their custom
definitions.")

(defmacro define-apropos-namespace (name (arg &optional keyword) &body body)
  "Defines a new namespace for use in APROPOS. ARG and BODY are the argument and
function body of the predicate that tests whether a given symbol names an object
in the target namespace; NAME is a string that is printed in the output of APROPOS
The KEYWORD argument is used for identifying the namespace in the system and is
automatically derived from the name if not provided."
  (require-type name 'string)
  (require-type keyword '(or null keyword))
  (unless keyword
    (setf keyword (make-keyword (string-upcase (substitute #\- #\Space name)))))
  `(setf (gethash ,keyword *apropos-namespaces*)
         (list ,name (nfunction ,(format nil "Apropos predicate for ~A" name)
                                (lambda (,arg) ,@body)))))

;;; APROPOS

(defun apropos (string &optional package)
  "Briefly describe all symbols which contain the specified STRING. If PACKAGE
is supplied then only describe symbols present in that package. If
EXTERNAL-ONLY then only describe external symbols in the specified package."
  (let ((symbols (apropos-list string package)))
    (map nil #'apropos-print symbols))
  (values))

(defun apropos-print (symbol)
  (loop for keyword being the hash-key of *apropos-namespaces*
        for (name predicate) = (gethash keyword *apropos-namespaces*)
        when (funcall predicate symbol)
          collect name into result
        finally (format t "~S ~@[(~{~A~^, ~})~]~%" symbol result)))

;;; Standard APROPOS definitions

(define-apropos-namespace "function" (x)
  (and (fboundp x)
       (not (macro-function x))
       (not (special-operator-p x))))

(define-apropos-namespace "compiler macro" (x)
  (and (fboundp x)
       (compiler-macro-function x)))

(define-apropos-namespace "macro" (x)
  (and (fboundp x)
       (macro-function x)
       (not (special-operator-p x))))

(define-apropos-namespace "special form" (x)
  (and (fboundp x)
       (special-operator-p x)))

(define-apropos-namespace "keyword" (x)
  (keywordp x))

(define-apropos-namespace "constant variable" (x)
  (and (constant-symbol-p x)
       (not (keywordp x))))

(define-apropos-namespace "static variable" (x)
  (and (static-variable-p x)
       (not (constant-symbol-p x))))

(define-apropos-namespace "dynamic variable" (x)
  (and (boundp x)
       (not (static-variable-p x))
       (not (constant-symbol-p x))))

(define-apropos-namespace "condition" (x)
  (and (find-class x nil)
       (subtypep (find-class x) (find-class 'condition))))

(define-apropos-namespace "class" (x)
  (and (find-class x nil)
       (not (subtypep (find-class x) (find-class 'condition)))))
