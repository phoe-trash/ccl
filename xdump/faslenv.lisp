;;;-*- Mode: Lisp; Package: CCL -*-
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

;; Compile-time environment for fasl dumper/loader.

; loader state istruct
(def-accessors (faslstate) %svref
  ()
  faslstate.faslfname
  faslstate.faslevec
  faslstate.faslecnt
  faslstate.faslfd
  faslstate.faslval
  faslstate.faslstr
  faslstate.oldfaslstr
  faslstate.faslerr
  faslstate.iobuffer
  faslstate.bufcount
  faslstate.faslversion
  faslstate.faslepush
  faslstate.faslgsymbols
  faslstate.fasldispatch)

(defmacro define-fasl-operation (symbol number &optional docstring)
  (let ((name (format nil "$FASL-~A" (symbol-name symbol))))
    `(defconstant ,(intern name) ,number ,docstring)))

(defmacro define-fasl-operations (&rest operations)
  (assert (< (length operations) #x8F) ()
          "Attempting to define more than 126 FASL operations.")
  (loop for (symbol args description) in operations
        for argstring = (if args (format nil "Arguments: ~{~A~^, ~}~%" args) "")
        for docstring = (format nil "~A~A" argstring description)
        for i from 0
        collect `(define-fasl-operation ,symbol ,i ,docstring) into result
        finally (return `(progn ,@result))))

#+phoe (indent:define-indentation define-fasl-operations (&rest (&whole 2 &rest 2)))
#+phoe (indent:define-indentation define-fasl-operation (&whole 2 4 4 &optional 2))

(define-fasl-operations
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; No-op
  (:noop ()
    "No-op. Nada. Zilch. Space filler. Nothing. Void. Emptiness. (VALUES).")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Primitive types
  (:nil ()
    "Make a NIL.")
  (:immediate ((n long))
    "Make an immediate value.")
  (:char ((code count))
    "Make a char from with the provided CODE.")
  (:fixnum ((value word))
    "Make a fixnum.")
  (:s32 ((value long))
    "Make a (SIGNED-BYTE 32).")
  (:s64 ((high long) (low long))
    "Make a (SIGNED-BYTE 64).")
  (:bignum ((count count))
    "Make a bignum from COUNT's digits.")
  (:single-float ((bits long))
    "Make a single float from its bit representation in BITS.")
  (:double-float ((high long) (low long))
    "Make a double float from its bit representation in HIGH and LOW.")
  (:ratio ((numerator expr) (denominator expr))
    "Make a ratio from NUMERATOR and DENOMINATOR.")
  (:complex ((real expr) (imaginary expr))
    "Make a complex number from REAL and IMAGINARY.")
  (:cons ((car expr) (cdr expr))
    "Make a cons with the provided CAR and CDR.")
  (:list* ((n count))
    "Make a dotted list with N+2 expressions. The value of the last CDR is the N+2nd expression.")
  (:list ((n count))
    "Make a list with N+1 expressions.")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Vectors and arrays
  (:bit-vector ((count count))
    "Make a (SIMPLE-ARRAY BIT COUNT).")
  (:s8-vector ((count count))
    "Make a (SIMPLE-ARRAY (SIGNED-BYTE 8) COUNT).")
  (:u8-vector ((count count))
    "Make a (SIMPLE-ARRAY (UNSIGNED-BYTE 8) COUNT).")
  (:s16-vector ((count count))
    "Make a (SIMPLE-ARRAY (SIGNED-BYTE 16) COUNT).")
  (:u16-vector ((count count))
    "Make a (SIMPLE-ARRAY (UNSIGNED-BYTE 16) COUNT).")
  (:s32-vector ((count count))
    "Make a (SIMPLE-ARRAY (SIGNED-BYTE 32) COUNT).")
  (:u32-vector ((count count))
    "Make a (SIMPLE-ARRAY (UNSIGNED-BYTE 32) COUNT).")
  (:s64-vector ((count count))
    "Make a (SIMPLE-ARRAY (SIGNED-BYTE 64) COUNT).")
  (:u64-vector ((count count))
    "Make a (SIMPLE-ARRAY (UNSIGNED-BYTE 64) COUNT).")
  (:single-float-vector ((count count))
    "Make a (SIMPLE-ARRAY SINGLE-FLOAT COUNT).")
  (:double-float-vector ((count count))
    "Make a (SIMPLE-ARRAY DOUBLE-FLOAT COUNT).")
  (:t-vector ((count count))
    "Make a (SIMPLE-ARRAY T COUNT).")
  (:vector ((count count))
    "Make a vector from a vector header.")
  (:array ((count count))
    "Make an array from an array header.")
  (:code-vector ((count count))
    "Make a code vector of COUNT words.")
  (:istruct ((count count))
    "Make an ISTRUCT with COUNT elements.")
  (:istruct-cell ((symbol expr))
    "Register SYMBOL in *ISTRUCT-CELLS*.")
  (:vgvec ((subtype byte) (n count))
    "Make a GVECTOR with the provided SUBTYPE and COUNT expressions.")
  (:vivec ((subtype byte) (n count))
    "Make an IVECTOR with the provided SUBTYPE and COUNT expressions.")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Functions
  (:clfun ((size count) (codesize count))
    "Make a concatenated Lisp function, in which the machine code and constants are both contained in the same underlying uvector.")
  (:function ((count count))
    "Make a function from the gvector of COUNT bytes.")
  (:symfn ((symbol expr))
    "Get the function globally fbound to SYMBOL.")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Definitions
  (:defun ((named-fn expr) (docstring expr))
    "Make a function from NAMED-FN with DOCSTRING.")
  (:defmacro ((named-fn expr) (docstring expr))
    "Make a macro from NAMED-FN with DOCSTRING.")
  (:defconstant ((name expr) (value expr) (docstring expr))
    "Make a constant named NAME with VALUE and DOCSTRING.")
  (:defparameter ((name expr) (value expr) (docstring expr))
    "Make a parameter named NAME with VALUE and DOCSTRING.")
  (:defvar ((name expr))
    "Make a variable named NAME without initializing it.")
  (:defvar-init ((name expr) (value expr) (docstring expr))
    "Make a variable named NAME with VALUE and DOCSTRING.")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Strings and packages
  (:str ((string vstring))
    "Make a string.")
  (:pkg ((name vstring))
    "Return the package with the provided NAME.")
  (:mksym ((name vstring))
    "Make an uninterned symbol named NAME.")
  (:mksym-special ((name vstring))
    "Make a globally special uninterned symbol named NAME.")
  (:pkg-intern ((name vstring))
    "Intern a symbol named NAME in the current package.")
  (:intern-special ((package expr))
    "Intern a globally special symbol named NAME in the current package.")
  (:intern ((package expr) (name vstring))
    "Intern a symbol named NAME in PACKAGE.")
  (:pkg-intern-special ((package expr) (name vstring))
    "Intern a globally special symbol named NAME in PACKAGE.")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Operations and side effects
  (:eval ((expr expr))
    "Eval EXPR and return its value.")
  (:prog1 ((first expr) (second expr))
    "Evaluate FIRST and then SECOND, then return the value of FIRST.")
  (:lfuncall ((lfun expr))
    "Funcall the lfun.")
  (:platform ((n expr))
    "Ensure that the file is loadable on platform N.")
  (:vetab-alloc ((count count))
    "Make a new expression table with COUNT slots. The current etab is lost.")
  (:veref ((index count))
    "Get the value from an etab slot.")
  (:src ((source expr))
    "Set *LOADING-FILE-SOURCE-FILE* to SOURCE.")
  (:toplevel-location ((location expr))
    "Set *LOADING-TOPLEVEL-LOCATION* to LOCATION.")
  (:end ()
    "Stop reading the FASL file."))

;;; COUNT is a variable-length encoding of an unsigned integer, written
;;; 7 bits per octet, the least significant bits written first and the most
;;; significant octet having bit 7 set, so 127 would be written as #x00 and
;;; 128 as #x00 #x81.
;;; STRING is a COUNT (string length) followed by COUNT octets of UTF-8 data.

(provide "FASLENV")

(defpackage #:ccl-fasl
  (:use #:common-lisp)
  (:export #:+fasl-epush-bit+
           #:+fasl-buffer-length+
           #:+fasl-file-id+))

(in-package #:ccl-fasl)

(defconstant +fasl-epush-bit+ 7
  "The FASL opcode bit that is true if the value should be pushed into etab.")

(defconstant +fasl-buffer-length+ 2048
  "Buffer size for reading FASL files.")

(defconstant +fasl-file-id+ #xff00
  "Magic number identifying the FASL file format.")
