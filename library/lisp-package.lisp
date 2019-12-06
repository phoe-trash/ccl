;;;-*-Mode: LISP; Package: ccl -*-
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

; lisp-package.lisp
; Define a lisp package that attempts to behave like CLTL-1.

(in-package "CCL")

(defpackage "LISP"
  (:use )
  (:import-from "COMMON-LISP"
                "&ALLOW-OTHER-KEYS"
                "&AUX"
                "&BODY"
                "&ENVIRONMENT"
                "&KEY"
                "&OPTIONAL"
                "&REST"
                "&WHOLE"
                "*"
                "**"
                "***"
                "*DEBUG-IO*"
                "*DEFAULT-PATHNAME-DEFAULTS*"
                "*ERROR-OUTPUT*"
                "*FEATURES*"
                "*LOAD-VERBOSE*"
                "*MACROEXPAND-HOOK*"
                "*PACKAGE*"
                "*PRINT-ARRAY*"
                "*PRINT-BASE*"
                "*PRINT-CASE*"
                "*PRINT-CIRCLE*"
                "*PRINT-ESCAPE*"
                "*PRINT-GENSYM*"
                "*PRINT-LENGTH*"
                "*PRINT-LEVEL*"
                "*PRINT-PRETTY*"
                "*PRINT-RADIX*"
                "*QUERY-IO*"
                "*RANDOM-STATE*"
                "*READ-BASE*"
                "*READ-DEFAULT-FLOAT-FORMAT*"
                "*READ-SUPPRESS*"
                "*READTABLE*"
                "*STANDARD-INPUT*"
                "*STANDARD-OUTPUT*"
                "*TERMINAL-IO*"
                "*TRACE-OUTPUT*"
                "+"
                "++"
                "+++"
                "-"
                "/"
                "//"
                "///"
                "/="
                "1+"
                "1-"
                "<"
                "<="
                "="
                ">"
                ">="
                "ABS"
                "ACONS"
                "ACOS"
                "ACOSH"
                "ADJOIN"
                "ADJUST-ARRAY"
                "ADJUSTABLE-ARRAY-P"
                "ALPHA-CHAR-P"
                "ALPHANUMERICP"
                "AND"
                "APPEND"
                "APPLY"
                "APROPOS"
                "APROPOS-LIST"
                "AREF"
                "ARRAY"
                "ARRAY-DIMENSION"
                "ARRAY-DIMENSION-LIMIT"
                "ARRAY-DIMENSIONS"
                "ARRAY-ELEMENT-TYPE"
                "ARRAY-HAS-FILL-POINTER-P"
                "ARRAY-IN-BOUNDS-P"
                "ARRAY-RANK"
                "ARRAY-RANK-LIMIT"
                "ARRAY-ROW-MAJOR-INDEX"
                "ARRAY-TOTAL-SIZE"
                "ARRAY-TOTAL-SIZE-LIMIT"
                "ARRAYP"
                "ASH"
                "ASIN"
                "ASINH"
                "ASSERT"
                "ASSOC"
                "ASSOC-IF"
                "ASSOC-IF-NOT"
                "ATAN"
                "ATANH"
                "ATOM"
                "BIGNUM"
                "BIT"
                "BIT-AND"
                "BIT-ANDC1"
                "BIT-ANDC2"
                "BIT-EQV"
                "BIT-IOR"
                "BIT-NAND"
                "BIT-NOR"
                "BIT-NOT"
                "BIT-ORC1"
                "BIT-ORC2"
                "BIT-VECTOR"
                "BIT-VECTOR-P"
                "BIT-XOR"
                "BLOCK"
                "BOOLE"
                "BOOLE-1"
                "BOOLE-2"
                "BOOLE-AND"
                "BOOLE-ANDC1"
                "BOOLE-ANDC2"
                "BOOLE-C1"
                "BOOLE-C2"
                "BOOLE-CLR"
                "BOOLE-EQV"
                "BOOLE-IOR"
                "BOOLE-NAND"
                "BOOLE-NOR"
                "BOOLE-ORC1"
                "BOOLE-ORC2"
                "BOOLE-SET"
                "BOOLE-XOR"
                "BOTH-CASE-P"
                "BOUNDP"
                "BREAK"
                "BUTLAST"
                "BYTE"
                "BYTE-POSITION"
                "BYTE-SIZE"
                "CAAAAR"
                "CAAADR"
                "CAAAR"
                "CAADAR"
                "CAADDR"
                "CAADR"
                "CAAR"
                "CADAAR"
                "CADADR"
                "CADAR"
                "CADDAR"
                "CADDDR"
                "CADDR"
                "CADR"
                "CALL-ARGUMENTS-LIMIT"
                "CAR"
                "CASE"
                "CATCH"
                "CCASE"
                "CDAAAR"
                "CDAADR"
                "CDAAR"
                "CDADAR"
                "CDADDR"
                "CDADR"
                "CDAR"
                "CDDAAR"
                "CDDADR"
                "CDDAR"
                "CDDDAR"
                "CDDDDR"
                "CDDDR"
                "CDDR"
                "CDR"
                "CEILING"
                "CERROR"
                "CHAR"
                "CHAR-CODE"
                "CHAR-CODE-LIMIT"
                "CHAR-DOWNCASE"
                "CHAR-EQUAL"
;                "CHAR-FONT"
                "CHAR-GREATERP"
                "CHAR-INT"
                "CHAR-LESSP"
                "CHAR-NAME"
                "CHAR-NOT-EQUAL"
                "CHAR-NOT-GREATERP"
                "CHAR-NOT-LESSP"
                "CHAR-UPCASE"
                "CHAR/="
                "CHAR<"
                "CHAR<="
                "CHAR="
                "CHAR>"
                "CHAR>="
                "CHARACTER"
                "CHARACTERP"
                "CHECK-TYPE"
                "CIS"
                "CLEAR-INPUT"
                "CLEAR-OUTPUT"
                "CLOSE"
                "CLRHASH"
                "CODE-CHAR"
                "COERCE"
                "COMPILATION-SPEED"
                "COMPILE"
                "COMPILE-FILE"
                "COMPILED-FUNCTION"
                "COMPILED-FUNCTION-P"
                "COMPLEX"
                "COMPLEXP"
                "CONCATENATE"
                "COND"
                "CONJUGATE"
                "CONS"
                "CONSP"
                "CONSTANTP"
                "COPY-ALIST"
                "COPY-LIST"
                "COPY-READTABLE"
                "COPY-SEQ"
                "COPY-SYMBOL"
                "COPY-TREE"
                "COS"
                "COSH"
                "COUNT"
                "COUNT-IF"
                "COUNT-IF-NOT"
                "CTYPECASE"
                "DECF"
                "DECLARATION"
                "DECLARE"
                "DECODE-FLOAT"
                "DECODE-UNIVERSAL-TIME"
                "DEFCONSTANT"
                "DEFINE-MODIFY-MACRO"
                "DEFMACRO"
                "DEFPARAMETER"
                "DEFSETF"
                "DEFSTRUCT"
                "DEFTYPE"
                "DEFUN"
                "DEFVAR"
                "DELETE"
                "DELETE-DUPLICATES"
                "DELETE-FILE"
                "DELETE-IF"
                "DELETE-IF-NOT"
                "DENOMINATOR"
                "DEPOSIT-FIELD"
                "DESCRIBE"
                "DIGIT-CHAR-P"
                "DIRECTORY"
                "DIRECTORY-NAMESTRING"
                "DISASSEMBLE"
                "DO"
                "DO*"
                "DO-ALL-SYMBOLS"
                "DO-EXTERNAL-SYMBOLS"
                "DO-SYMBOLS"
                "DOCUMENTATION"
                "DOLIST"
                "DOTIMES"
                "DOUBLE-FLOAT"
                "DOUBLE-FLOAT-EPSILON"
                "DOUBLE-FLOAT-NEGATIVE-EPSILON"
                "DPB"
                "DRIBBLE"
                "ECASE"
                "ED"
                "EIGHTH"
                "ELT"
                "ENCODE-UNIVERSAL-TIME"
                "ENDP"
                "ENOUGH-NAMESTRING"
                "EQ"
                "EQL"
                "EQUAL"
                "EQUALP"
                "ERROR"
                "ETYPECASE"
                "EVAL"
                "EVAL-WHEN"
                "EVENP"
                "EVERY"
                "EXP"
                "EXPT"
                "FBOUNDP"
                "FCEILING"
                "FFLOOR"
                "FIFTH"
                "FILE-AUTHOR"
                "FILE-LENGTH"
                "FILE-NAMESTRING"
                "FILE-POSITION"
                "FILE-WRITE-DATE"
                "FILL"
                "FILL-POINTER"
                "FIND"
                "FIND-ALL-SYMBOLS"
                "FIND-IF"
                "FIND-IF-NOT"
                "FIND-PACKAGE"
                "FIND-SYMBOL"
                "FINISH-OUTPUT"
                "FIRST"
                "FIXNUM"
                "FLET"
                "FLOAT"
                "FLOAT-DIGITS"
                "FLOAT-PRECISION"
                "FLOAT-RADIX"
                "FLOAT-SIGN"
                "FLOATP"
                "FLOOR"
                "FMAKUNBOUND"
                "FORCE-OUTPUT"
                "FORMAT"
                "FOURTH"
                "FRESH-LINE"
                "FROUND"
                "FTRUNCATE"
                "FTYPE"
                "FUNCALL"
                "FUNCTION"
                "GCD"
                "GENSYM"
                "GENTEMP"
                "GET"
                "GET-DECODED-TIME"
                "GET-DISPATCH-MACRO-CHARACTER"
                "GET-INTERNAL-REAL-TIME"
                "GET-INTERNAL-RUN-TIME"
                "GET-MACRO-CHARACTER"
                "GET-OUTPUT-STREAM-STRING"
                "GET-PROPERTIES"
                "GET-UNIVERSAL-TIME"
                "GETF"
                "GETHASH"
                "GO"
                "GRAPHIC-CHAR-P"
                "HASH-TABLE"
                "HASH-TABLE-COUNT"
                "HASH-TABLE-P"
                "HOST-NAMESTRING"
                "IDENTITY"
                "IF"
                "IGNORE"
                "IMAGPART"
                "INCF"
                "INLINE"
                "INPUT-STREAM-P"
                "INSPECT"
                "INTEGER"
                "INTEGER-DECODE-FLOAT"
                "INTEGER-LENGTH"
                "INTEGERP"
                "INTERN"
                "INTERNAL-TIME-UNITS-PER-SECOND"
                "INTERSECTION"
                "ISQRT"
                "KEYWORD"
                "KEYWORDP"
                "LABELS"
                "LAMBDA"
                "LAMBDA-LIST-KEYWORDS"
                "LAMBDA-PARAMETERS-LIMIT"
                "LAST"
                "LCM"
                "LDB"
                "LDB-TEST"
                "LDIFF"
                "LEAST-NEGATIVE-DOUBLE-FLOAT"
                "LEAST-NEGATIVE-LONG-FLOAT"
                "LEAST-NEGATIVE-SHORT-FLOAT"
                "LEAST-NEGATIVE-SINGLE-FLOAT"
                "LEAST-POSITIVE-DOUBLE-FLOAT"
                "LEAST-POSITIVE-LONG-FLOAT"
                "LEAST-POSITIVE-SHORT-FLOAT"
                "LEAST-POSITIVE-SINGLE-FLOAT"
                "LENGTH"
                "LET"
                "LET*"
                "LISP-IMPLEMENTATION-TYPE"
                "LISP-IMPLEMENTATION-VERSION"
                "LIST"
                "LIST*"
                "LIST-ALL-PACKAGES"
                "LIST-LENGTH"
                "LISTEN"
                "LISTP"
                "LOAD"
                "LOCALLY"
                "LOG"
                "LOGAND"
                "LOGANDC1"
                "LOGANDC2"
                "LOGBITP"
                "LOGCOUNT"
                "LOGEQV"
                "LOGIOR"
                "LOGNAND"
                "LOGNOR"
                "LOGNOT"
                "LOGORC1"
                "LOGORC2"
                "LOGTEST"
                "LOGXOR"
                "LONG-FLOAT"
                "LONG-FLOAT-EPSILON"
                "LONG-FLOAT-NEGATIVE-EPSILON"
                "LONG-SITE-NAME"
                "LOOP"
                "LOWER-CASE-P"
                "MACHINE-INSTANCE"
                "MACHINE-TYPE"
                "MACHINE-VERSION"
                "MACRO-FUNCTION"
                "MACROEXPAND"
                "MACROEXPAND-1"
                "MACROLET"
                "MAKE-ARRAY"
                "MAKE-BROADCAST-STREAM"
                "MAKE-CONCATENATED-STREAM"
                "MAKE-DISPATCH-MACRO-CHARACTER"
                "MAKE-ECHO-STREAM"
                "MAKE-HASH-TABLE"
                "MAKE-LIST"
                "MAKE-PATHNAME"
                "MAKE-RANDOM-STATE"
                "MAKE-SEQUENCE"
                "MAKE-STRING"
                "MAKE-STRING-INPUT-STREAM"
                "MAKE-STRING-OUTPUT-STREAM"
                "MAKE-SYMBOL"
                "MAKE-SYNONYM-STREAM"
                "MAKE-TWO-WAY-STREAM"
                "MAKUNBOUND"
                "MAP"
                "MAPC"
                "MAPCAN"
                "MAPCAR"
                "MAPCON"
                "MAPHASH"
                "MAPL"
                "MAPLIST"
                "MASK-FIELD"
                "MAX"
                "MEMBER"
                "MEMBER-IF"
                "MEMBER-IF-NOT"
                "MERGE"
                "MERGE-PATHNAMES"
                "MIN"
                "MINUSP"
                "MISMATCH"
                "MOD"
                "MOST-NEGATIVE-DOUBLE-FLOAT"
                "MOST-NEGATIVE-FIXNUM"
                "MOST-NEGATIVE-LONG-FLOAT"
                "MOST-NEGATIVE-SHORT-FLOAT"
                "MOST-NEGATIVE-SINGLE-FLOAT"
                "MOST-POSITIVE-DOUBLE-FLOAT"
                "MOST-POSITIVE-FIXNUM"
                "MOST-POSITIVE-LONG-FLOAT"
                "MOST-POSITIVE-SHORT-FLOAT"
                "MOST-POSITIVE-SINGLE-FLOAT"
                "MULTIPLE-VALUE-BIND"
                "MULTIPLE-VALUE-CALL"
                "MULTIPLE-VALUE-LIST"
                "MULTIPLE-VALUE-PROG1"
                "MULTIPLE-VALUE-SETQ"
                "MULTIPLE-VALUES-LIMIT"
                "NAME-CHAR"
                "NAMESTRING"
                "NBUTLAST"
                "NCONC"
                "NIL"
                "NINTERSECTION"
                "NINTH"
                "NOT"
                "NOTANY"
                "NOTEVERY"
                "NOTINLINE"
                "NRECONC"
                "NREVERSE"
                "NSET-DIFFERENCE"
                "NSET-EXCLUSIVE-OR"
                "NSTRING-CAPITALIZE"
                "NSTRING-DOWNCASE"
                "NSTRING-UPCASE"
                "NSUBLIS"
                "NSUBST"
                "NSUBST-IF"
                "NSUBST-IF-NOT"
                "NSUBSTITUTE"
                "NSUBSTITUTE-IF"
                "NSUBSTITUTE-IF-NOT"
                "NTH"
                "NTHCDR"
                "NULL"
                "NUMBER"
                "NUMBERP"
                "NUMERATOR"
                "NUNION"
                "ODDP"
                "OPEN"
                "OPTIMIZE"
                "OR"
                "OTHERWISE"
                "OUTPUT-STREAM-P"
                "PACKAGE"
                "PACKAGE-NAME"
                "PACKAGE-NICKNAMES"
                "PACKAGE-SHADOWING-SYMBOLS"
                "PACKAGE-USE-LIST"
                "PACKAGE-USED-BY-LIST"
                "PACKAGEP"
                "PAIRLIS"
                "PARSE-INTEGER"
                "PARSE-NAMESTRING"
                "PATHNAME"
                "PATHNAME-DEVICE"
                "PATHNAME-DIRECTORY"
                "PATHNAME-HOST"
                "PATHNAME-NAME"
                "PATHNAME-TYPE"
                "PATHNAME-VERSION"
                "PATHNAMEP"
                "PEEK-CHAR"
                "PHASE"
                "PI"
                "PLUSP"
                "POP"
                "POSITION"
                "POSITION-IF"
                "POSITION-IF-NOT"
                "PPRINT"
                "PRIN1"
                "PRIN1-TO-STRING"
                "PRINC"
                "PRINC-TO-STRING"
                "PRINT"
                "PROBE-FILE"
                "PROCLAIM"
                "PROG"
                "PROG*"
                "PROG1"
                "PROG2"
                "PROGN"
                "PROGV"
                "PSETF"
                "PSETQ"
                "PUSH"
                "PUSHNEW"
                "QUOTE"
                "RANDOM"
                "RANDOM-STATE"
                "RANDOM-STATE-P"
                "RASSOC"
                "RASSOC-IF"
                "RASSOC-IF-NOT"
                "RATIO"
                "RATIONAL"
                "RATIONALIZE"
                "RATIONALP"
                "READ"
                "READ-BYTE"
                "READ-CHAR"
                "READ-CHAR-NO-HANG"
                "READ-DELIMITED-LIST"
                "READ-FROM-STRING"
                "READ-LINE"
                "READ-PRESERVING-WHITESPACE"
                "READTABLE"
                "READTABLEP"
                "REALPART"
                "REDUCE"
                "REM"
                "REMF"
                "REMHASH"
                "REMOVE"
                "REMOVE-DUPLICATES"
                "REMOVE-IF"
                "REMOVE-IF-NOT"
                "REMPROP"
                "RENAME-FILE"
                "RENAME-PACKAGE"
                "REPLACE"
                "REST"
                "RETURN"
                "RETURN-FROM"
                "REVAPPEND"
                "REVERSE"
                "ROOM"
                "ROTATEF"
                "ROUND"
                "RPLACA"
                "RPLACD"
                "SAFETY"
                "SATISFIES"
                "SBIT"
                "SCALE-FLOAT"
                "SCHAR"
                "SEARCH"
                "SECOND"
                "SEQUENCE"
                "SET"
;                "SET-CHAR-BIT"
                "SET-DIFFERENCE"
                "SET-DISPATCH-MACRO-CHARACTER"
                "SET-EXCLUSIVE-OR"
                "SET-MACRO-CHARACTER"
                "SET-SYNTAX-FROM-CHAR"
                "SETF"
                "SETQ"
                "SEVENTH"
                "SHIFTF"
                "SHORT-FLOAT"
                "SHORT-FLOAT-EPSILON"
                "SHORT-FLOAT-NEGATIVE-EPSILON"
                "SHORT-SITE-NAME"
                "SIGNED-BYTE"
                "SIGNUM"
                "SIMPLE-ARRAY"
                "SIMPLE-BIT-VECTOR"
                "SIMPLE-BIT-VECTOR-P"
                "SIMPLE-STRING"
                "SIMPLE-STRING-P"
                "SIMPLE-VECTOR"
                "SIMPLE-VECTOR-P"
                "SIN"
                "SINGLE-FLOAT"
                "SINGLE-FLOAT-EPSILON"
                "SINGLE-FLOAT-NEGATIVE-EPSILON"
                "SINH"
                "SIXTH"
                "SLEEP"
                "SOFTWARE-TYPE"
                "SOFTWARE-VERSION"
                "SOME"
                "SORT"
                "SPACE"
                "SPECIAL"
                "SPEED"
                "SQRT"
                "STABLE-SORT"
                "STANDARD-CHAR"
                "STANDARD-CHAR-P"
                "STEP"
                "STREAM"
                "STREAM-ELEMENT-TYPE"
                "STREAMP"
                "STRING"
                "STRING-CAPITALIZE"
;                "STRING-CHAR"
;                "STRING-CHAR-P"
                "STRING-DOWNCASE"
                "STRING-EQUAL"
                "STRING-GREATERP"
                "STRING-LEFT-TRIM"
                "STRING-LESSP"
                "STRING-NOT-EQUAL"
                "STRING-NOT-GREATERP"
                "STRING-NOT-LESSP"
                "STRING-RIGHT-TRIM"
                "STRING-TRIM"
                "STRING-UPCASE"
                "STRING/="
                "STRING<"
                "STRING<="
                "STRING="
                "STRING>"
                "STRING>="
                "STRINGP"
                "STRUCTURE"
                "SUBLIS"
                "SUBSEQ"
                "SUBSETP"
                "SUBST"
                "SUBST-IF"
                "SUBST-IF-NOT"
                "SUBSTITUTE"
                "SUBSTITUTE-IF"
                "SUBSTITUTE-IF-NOT"
                "SUBTYPEP"
                "SVREF"
                "SXHASH"
                "SYMBOL"
                "SYMBOL-FUNCTION"
                "SYMBOL-NAME"
                "SYMBOL-PACKAGE"
                "SYMBOL-PLIST"
                "SYMBOL-VALUE"
                "SYMBOLP"
                "T"
                "TAGBODY"
                "TAILP"
                "TAN"
                "TANH"
                "TENTH"
                "TERPRI"
                "THE"
                "THIRD"
                "THROW"
                "TIME"
                "TRACE"
                "TREE-EQUAL"
                "TRUENAME"
                "TRUNCATE"
                "TYPE"
                "TYPE-OF"
                "TYPECASE"
                "TYPEP"
                "UNINTERN"
                "UNION"
                "UNLESS"
                "UNREAD-CHAR"
                "UNSIGNED-BYTE"
                "UNTRACE"
                "UNWIND-PROTECT"
                "UPPER-CASE-P"
                "USER-HOMEDIR-PATHNAME"
                "VALUES"
                "VALUES-LIST"
                "VARIABLE"
                "VECTOR"
                "VECTOR-POP"
                "VECTOR-PUSH"
                "VECTOR-PUSH-EXTEND"
                "VECTORP"
                "WARN"
                "WHEN"
                "WITH-INPUT-FROM-STRING"
                "WITH-OPEN-FILE"
                "WITH-OPEN-STREAM"
                "WITH-OUTPUT-TO-STRING"
                "WRITE"
                "WRITE-BYTE"
                "WRITE-CHAR"
                "WRITE-LINE"
                "WRITE-STRING"
                "WRITE-TO-STRING"
                "Y-OR-N-P"
                "YES-OR-NO-P"
                "ZEROP"
		"*MODULES*"
		"PROVIDE"
		"REQUIRE")
  (:import-from "CCL"
                "*BREAK-ON-WARNINGS*"
                "COMPILER-LET"
		"*APPLYHOOK*"
		"*EVALHOOK*"
		"APPLYHOOK"
		"EVALHOOK"
		"SPECIAL-FORM-P"
)
  (:shadow "IN-PACKAGE"
           "FUNCTIONP"
           "MAKE-PACKAGE"
           "SHADOW"
           "SHADOWING-IMPORT"
           "EXPORT"
           "UNEXPORT"
           "USE-PACKAGE"
           "UNUSE-PACKAGE"
           "IMPORT")
  (:export
   "&ALLOW-OTHER-KEYS"
   "&AUX"
   "&BODY"
   "&ENVIRONMENT"
   "&KEY"
   "&OPTIONAL"
   "&REST"
   "&WHOLE"
   "*"
   "**"
   "***"
   "*APPLYHOOK*"
   "*BREAK-ON-WARNINGS*"
   "*DEBUG-IO*"
   "*DEFAULT-PATHNAME-DEFAULTS*"
   "*ERROR-OUTPUT*"
   "*EVALHOOK*"
   "*FEATURES*"
   "*LOAD-VERBOSE*"
   "*MODULES*"
   "*MACROEXPAND-HOOK*"
   "*PACKAGE*"
   "*PRINT-ARRAY*"
   "*PRINT-BASE*"
   "*PRINT-CASE*"
   "*PRINT-CIRCLE*"
   "*PRINT-ESCAPE*"
   "*PRINT-GENSYM*"
   "*PRINT-LENGTH*"
   "*PRINT-LEVEL*"
   "*PRINT-PRETTY*"
   "*PRINT-RADIX*"
   "*QUERY-IO*"
   "*RANDOM-STATE*"
   "*READ-BASE*"
   "*READ-DEFAULT-FLOAT-FORMAT*"
   "*READ-SUPPRESS*"
   "*READTABLE*"
   "*STANDARD-INPUT*"
   "*STANDARD-OUTPUT*"
   "*TERMINAL-IO*"
   "*TRACE-OUTPUT*"
   "+"
   "++"
   "+++"
   "-"
   "/"
   "//"
   "///"
   "/="
   "1+"
   "1-"
   "<"
   "<="
   "="
   ">"
   ">="
   "ABS"
   "ACONS"
   "ACOS"
   "ACOSH"
   "ADJOIN"
   "ADJUST-ARRAY"
   "ADJUSTABLE-ARRAY-P"
   "ALPHA-CHAR-P"
   "ALPHANUMERICP"
   "AND"
   "APPEND"
   "APPLY"
   "APPLYHOOK"
   "APROPOS"
   "APROPOS-LIST"
   "AREF"
   "ARRAY"
   "ARRAY-DIMENSION"
   "ARRAY-DIMENSION-LIMIT"
   "ARRAY-DIMENSIONS"
   "ARRAY-ELEMENT-TYPE"
   "ARRAY-HAS-FILL-POINTER-P"
   "ARRAY-IN-BOUNDS-P"
   "ARRAY-RANK"
   "ARRAY-RANK-LIMIT"
   "ARRAY-ROW-MAJOR-INDEX"
   "ARRAY-TOTAL-SIZE"
   "ARRAY-TOTAL-SIZE-LIMIT"
   "ARRAYP"
   "ASH"
   "ASIN"
   "ASINH"
   "ASSERT"
   "ASSOC"
   "ASSOC-IF"
   "ASSOC-IF-NOT"
   "ATAN"
   "ATANH"
   "ATOM"
   "BIGNUM"
   "BIT"
   "BIT-AND"
   "BIT-ANDC1"
   "BIT-ANDC2"
   "BIT-EQV"
   "BIT-IOR"
   "BIT-NAND"
   "BIT-NOR"
   "BIT-NOT"
   "BIT-ORC1"
   "BIT-ORC2"
   "BIT-VECTOR"
   "BIT-VECTOR-P"
   "BIT-XOR"
   "BLOCK"
   "BOOLE"
   "BOOLE-1"
   "BOOLE-2"
   "BOOLE-AND"
   "BOOLE-ANDC1"
   "BOOLE-ANDC2"
   "BOOLE-C1"
   "BOOLE-C2"
   "BOOLE-CLR"
   "BOOLE-EQV"
   "BOOLE-IOR"
   "BOOLE-NAND"
   "BOOLE-NOR"
   "BOOLE-ORC1"
   "BOOLE-ORC2"
   "BOOLE-SET"
   "BOOLE-XOR"
   "BOTH-CASE-P"
   "BOUNDP"
   "BREAK"
   "BUTLAST"
   "BYTE"
   "BYTE-POSITION"
   "BYTE-SIZE"
   "CAAAAR"
   "CAAADR"
   "CAAAR"
   "CAADAR"
   "CAADDR"
   "CAADR"
   "CAAR"
   "CADAAR"
   "CADADR"
   "CADAR"
   "CADDAR"
   "CADDDR"
   "CADDR"
   "CADR"
   "CALL-ARGUMENTS-LIMIT"
   "CAR"
   "CASE"
   "CATCH"
   "CCASE"
   "CDAAAR"
   "CDAADR"
   "CDAAR"
   "CDADAR"
   "CDADDR"
   "CDADR"
   "CDAR"
   "CDDAAR"
   "CDDADR"
   "CDDAR"
   "CDDDAR"
   "CDDDDR"
   "CDDDR"
   "CDDR"
   "CDR"
   "CEILING"
   "CERROR"
   "CHAR"
   "CHAR-BIT"
   "CHAR-BITS"
   "CHAR-BITS-LIMIT"
   "CHAR-CODE"
   "CHAR-CODE-LIMIT"
   "CHAR-CONTROL-BIT"
   "CHAR-DOWNCASE"
   "CHAR-EQUAL"
   "CHAR-FONT"
   "CHAR-FONT-LIMIT"
   "CHAR-GREATERP"
   "CHAR-HYPER-BIT"
   "CHAR-INT"
   "CHAR-LESSP"
   "CHAR-META-BIT"
   "CHAR-NAME"
   "CHAR-NOT-EQUAL"
   "CHAR-NOT-GREATERP"
   "CHAR-NOT-LESSP"
   "CHAR-SUPER-BIT"
   "CHAR-UPCASE"
   "CHAR/="
   "CHAR<"
   "CHAR<="
   "CHAR="
   "CHAR>"
   "CHAR>="
   "CHARACTER"
   "CHARACTERP"
   "CHECK-TYPE"
   "CIS"
   "CLEAR-INPUT"
   "CLEAR-OUTPUT"
   "CLOSE"
   "CLRHASH"
   "CODE-CHAR"
   "COERCE"
   "COMMON"
   "COMMONP"
   "COMPILATION-SPEED"
   "COMPILE"
   "COMPILE-FILE"
   "COMPILED-FUNCTION"
   "COMPILED-FUNCTION-P"
   "COMPILER-LET"
   "COMPLEX"
   "COMPLEXP"
   "CONCATENATE"
   "COND"
   "CONJUGATE"
   "CONS"
   "CONSP"
   "CONSTANTP"
   "COPY-ALIST"
   "COPY-LIST"
   "COPY-READTABLE"
   "COPY-SEQ"
   "COPY-SYMBOL"
   "COPY-TREE"
   "COS"
   "COSH"
   "COUNT"
   "COUNT-IF"
   "COUNT-IF-NOT"
   "CTYPECASE"
   "DECF"
   "DECLARATION"
   "DECLARE"
   "DECODE-FLOAT"
   "DECODE-UNIVERSAL-TIME"
   "DEFCONSTANT"
   "DEFINE-MODIFY-MACRO"
   "DEFINE-SETF-METHOD"
   "DEFMACRO"
   "DEFPARAMETER"
   "DEFSETF"
   "DEFSTRUCT"
   "DEFTYPE"
   "DEFUN"
   "DEFVAR"
   "DELETE"
   "DELETE-DUPLICATES"
   "DELETE-FILE"
   "DELETE-IF"
   "DELETE-IF-NOT"
   "DENOMINATOR"
   "DEPOSIT-FIELD"
   "DESCRIBE"
   "DIGIT-CHAR"
   "DIGIT-CHAR-P"
   "DIRECTORY"
   "DIRECTORY-NAMESTRING"
   "DISASSEMBLE"
   "DO"
   "DO*"
   "DO-ALL-SYMBOLS"
   "DO-EXTERNAL-SYMBOLS"
   "DO-SYMBOLS"
   "DOCUMENTATION"
   "DOLIST"
   "DOTIMES"
   "DOUBLE-FLOAT"
   "DOUBLE-FLOAT-EPSILON"
   "DOUBLE-FLOAT-NEGATIVE-EPSILON"
   "DPB"
   "DRIBBLE"
   "ECASE"
   "ED"
   "EIGHTH"
   "ELT"
   "ENCODE-UNIVERSAL-TIME"
   "ENDP"
   "ENOUGH-NAMESTRING"
   "EQ"
   "EQL"
   "EQUAL"
   "EQUALP"
   "ERROR"
   "ETYPECASE"
   "EVAL"
   "EVAL-WHEN"
   "EVALHOOK"
   "EVENP"
   "EVERY"
   "EXP"
   "EXPORT"
   "EXPT"
   "FBOUNDP"
   "FCEILING"
   "FFLOOR"
   "FIFTH"
   "FILE-AUTHOR"
   "FILE-LENGTH"
   "FILE-NAMESTRING"
   "FILE-POSITION"
   "FILE-WRITE-DATE"
   "FILL"
   "FILL-POINTER"
   "FIND"
   "FIND-ALL-SYMBOLS"
   "FIND-IF"
   "FIND-IF-NOT"
   "FIND-PACKAGE"
   "FIND-SYMBOL"
   "FINISH-OUTPUT"
   "FIRST"
   "FIXNUM"
   "FLET"
   "FLOAT"
   "FLOAT-DIGITS"
   "FLOAT-PRECISION"
   "FLOAT-RADIX"
   "FLOAT-SIGN"
   "FLOATP"
   "FLOOR"
   "FMAKUNBOUND"
   "FORCE-OUTPUT"
   "FORMAT"
   "FOURTH"
   "FRESH-LINE"
   "FROUND"
   "FTRUNCATE"
   "FTYPE"
   "FUNCALL"
   "FUNCTION"
   "FUNCTIONP"
   "GCD"
   "GENSYM"
   "GENTEMP"
   "GET"
   "GET-DECODED-TIME"
   "GET-DISPATCH-MACRO-CHARACTER"
   "GET-INTERNAL-REAL-TIME"
   "GET-INTERNAL-RUN-TIME"
   "GET-MACRO-CHARACTER"
   "GET-OUTPUT-STREAM-STRING"
   "GET-PROPERTIES"
   "GET-SETF-METHOD"
   "GET-SETF-METHOD-MULTIPLE-VALUE"
   "GET-UNIVERSAL-TIME"
   "GETF"
   "GETHASH"
   "GO"
   "GRAPHIC-CHAR-P"
   "HASH-TABLE"
   "HASH-TABLE-COUNT"
   "HASH-TABLE-P"
   "HOST-NAMESTRING"
   "IDENTITY"
   "IF"
   "IGNORE"
   "IMAGPART"
   "IMPORT"
   "IN-PACKAGE"
   "INCF"
   "INLINE"
   "INPUT-STREAM-P"
   "INSPECT"
   "INT-CHAR"
   "INTEGER"
   "INTEGER-DECODE-FLOAT"
   "INTEGER-LENGTH"
   "INTEGERP"
   "INTERN"
   "INTERNAL-TIME-UNITS-PER-SECOND"
   "INTERSECTION"
   "ISQRT"
   "KEYWORD"
   "KEYWORDP"
   "LABELS"
   "LAMBDA"
   "LAMBDA-LIST-KEYWORDS"
   "LAMBDA-PARAMETERS-LIMIT"
   "LAST"
   "LCM"
   "LDB"
   "LDB-TEST"
   "LDIFF"
   "LEAST-NEGATIVE-DOUBLE-FLOAT"
   "LEAST-NEGATIVE-LONG-FLOAT"
   "LEAST-NEGATIVE-SHORT-FLOAT"
   "LEAST-NEGATIVE-SINGLE-FLOAT"
   "LEAST-POSITIVE-DOUBLE-FLOAT"
   "LEAST-POSITIVE-LONG-FLOAT"
   "LEAST-POSITIVE-SHORT-FLOAT"
   "LEAST-POSITIVE-SINGLE-FLOAT"
   "LENGTH"
   "LET"
   "LET*"
   "LISP-IMPLEMENTATION-TYPE"
   "LISP-IMPLEMENTATION-VERSION"
   "LIST"
   "LIST*"
   "LIST-ALL-PACKAGES"
   "LIST-LENGTH"
   "LISTEN"
   "LISTP"
   "LOAD"
   "LOCALLY"
   "LOG"
   "LOGAND"
   "LOGANDC1"
   "LOGANDC2"
   "LOGBITP"
   "LOGCOUNT"
   "LOGEQV"
   "LOGIOR"
   "LOGNAND"
   "LOGNOR"
   "LOGNOT"
   "LOGORC1"
   "LOGORC2"
   "LOGTEST"
   "LOGXOR"
   "LONG-FLOAT"
   "LONG-FLOAT-EPSILON"
   "LONG-FLOAT-NEGATIVE-EPSILON"
   "LONG-SITE-NAME"
   "LOOP"
   "LOWER-CASE-P"
   "MACHINE-INSTANCE"
   "MACHINE-TYPE"
   "MACHINE-VERSION"
   "MACRO-FUNCTION"
   "MACROEXPAND"
   "MACROEXPAND-1"
   "MACROLET"
   "MAKE-ARRAY"
   "MAKE-BROADCAST-STREAM"
   "MAKE-CHAR"
   "MAKE-CONCATENATED-STREAM"
   "MAKE-DISPATCH-MACRO-CHARACTER"
   "MAKE-ECHO-STREAM"
   "MAKE-HASH-TABLE"
   "MAKE-LIST"
   "MAKE-PACKAGE"
   "MAKE-PATHNAME"
   "MAKE-RANDOM-STATE"
   "MAKE-SEQUENCE"
   "MAKE-STRING"
   "MAKE-STRING-INPUT-STREAM"
   "MAKE-STRING-OUTPUT-STREAM"
   "MAKE-SYMBOL"
   "MAKE-SYNONYM-STREAM"
   "MAKE-TWO-WAY-STREAM"
   "MAKUNBOUND"
   "MAP"
   "MAPC"
   "MAPCAN"
   "MAPCAR"
   "MAPCON"
   "MAPHASH"
   "MAPL"
   "MAPLIST"
   "MASK-FIELD"
   "MAX"
   "MEMBER"
   "MEMBER-IF"
   "MEMBER-IF-NOT"
   "MERGE"
   "MERGE-PATHNAMES"
   "MIN"
   "MINUSP"
   "MISMATCH"
   "MOD"
   "MOST-NEGATIVE-DOUBLE-FLOAT"
   "MOST-NEGATIVE-FIXNUM"
   "MOST-NEGATIVE-LONG-FLOAT"
   "MOST-NEGATIVE-SHORT-FLOAT"
   "MOST-NEGATIVE-SINGLE-FLOAT"
   "MOST-POSITIVE-DOUBLE-FLOAT"
   "MOST-POSITIVE-FIXNUM"
   "MOST-POSITIVE-LONG-FLOAT"
   "MOST-POSITIVE-SHORT-FLOAT"
   "MOST-POSITIVE-SINGLE-FLOAT"
   "MULTIPLE-VALUE-BIND"
   "MULTIPLE-VALUE-CALL"
   "MULTIPLE-VALUE-LIST"
   "MULTIPLE-VALUE-PROG1"
   "MULTIPLE-VALUE-SETQ"
   "MULTIPLE-VALUES-LIMIT"
   "NAME-CHAR"
   "NAMESTRING"
   "NBUTLAST"
   "NCONC"
   "NIL"
   "NINTERSECTION"
   "NINTH"
   "NOT"
   "NOTANY"
   "NOTEVERY"
   "NOTINLINE"
   "NRECONC"
   "NREVERSE"
   "NSET-DIFFERENCE"
   "NSET-EXCLUSIVE-OR"
   "NSTRING-CAPITALIZE"
   "NSTRING-DOWNCASE"
   "NSTRING-UPCASE"
   "NSUBLIS"
   "NSUBST"
   "NSUBST-IF"
   "NSUBST-IF-NOT"
   "NSUBSTITUTE"
   "NSUBSTITUTE-IF"
   "NSUBSTITUTE-IF-NOT"
   "NTH"
   "NTHCDR"
   "NULL"
   "NUMBER"
   "NUMBERP"
   "NUMERATOR"
   "NUNION"
   "ODDP"
   "OPEN"
   "OPTIMIZE"
   "OR"
   "OTHERWISE"
   "OUTPUT-STREAM-P"
   "PACKAGE"
   "PACKAGE-NAME"
   "PACKAGE-NICKNAMES"
   "PACKAGE-SHADOWING-SYMBOLS"
   "PACKAGE-USE-LIST"
   "PACKAGE-USED-BY-LIST"
   "PACKAGEP"
   "PAIRLIS"
   "PARSE-INTEGER"
   "PARSE-NAMESTRING"
   "PATHNAME"
   "PATHNAME-DEVICE"
   "PATHNAME-DIRECTORY"
   "PATHNAME-HOST"
   "PATHNAME-NAME"
   "PATHNAME-TYPE"
   "PATHNAME-VERSION"
   "PATHNAMEP"
   "PEEK-CHAR"
   "PHASE"
   "PI"
   "PLUSP"
   "POP"
   "POSITION"
   "POSITION-IF"
   "POSITION-IF-NOT"
   "PPRINT"
   "PRIN1"
   "PRIN1-TO-STRING"
   "PRINC"
   "PRINC-TO-STRING"
   "PRINT"
   "PROBE-FILE"
   "PROCLAIM"
   "PROG"
   "PROG*"
   "PROG1"
   "PROG2"
   "PROGN"
   "PROGV"
   "PROVIDE"
   "PSETF"
   "PSETQ"
   "PUSH"
   "PUSHNEW"
   "QUOTE"
   "RANDOM"
   "RANDOM-STATE"
   "RANDOM-STATE-P"
   "RASSOC"
   "RASSOC-IF"
   "RASSOC-IF-NOT"
   "RATIO"
   "RATIONAL"
   "RATIONALIZE"
   "RATIONALP"
   "READ"
   "READ-BYTE"
   "READ-CHAR"
   "READ-CHAR-NO-HANG"
   "READ-DELIMITED-LIST"
   "READ-FROM-STRING"
   "READ-LINE"
   "READ-PRESERVING-WHITESPACE"
   "READTABLE"
   "READTABLEP"
   "REALPART"
   "REDUCE"
   "REM"
   "REMF"
   "REMHASH"
   "REMOVE"
   "REMOVE-DUPLICATES"
   "REMOVE-IF"
   "REMOVE-IF-NOT"
   "REMPROP"
   "RENAME-FILE"
   "RENAME-PACKAGE"
   "REPLACE"
   "REQUIRE"
   "REST"
   "RETURN"
   "RETURN-FROM"
   "REVAPPEND"
   "REVERSE"
   "ROOM"
   "ROTATEF"
   "ROUND"
   "RPLACA"
   "RPLACD"
   "SAFETY"
   "SATISFIES"
   "SBIT"
   "SCALE-FLOAT"
   "SCHAR"
   "SEARCH"
   "SECOND"
   "SEQUENCE"
   "SET"
   "SET-CHAR-BIT"
   "SET-DIFFERENCE"
   "SET-DISPATCH-MACRO-CHARACTER"
   "SET-EXCLUSIVE-OR"
   "SET-MACRO-CHARACTER"
   "SET-SYNTAX-FROM-CHAR"
   "SETF"
   "SETQ"
   "SEVENTH"
   "SHADOW"
   "SHADOWING-IMPORT"
   "SHIFTF"
   "SHORT-FLOAT"
   "SHORT-FLOAT-EPSILON"
   "SHORT-FLOAT-NEGATIVE-EPSILON"
   "SHORT-SITE-NAME"
   "SIGNED-BYTE"
   "SIGNUM"
   "SIMPLE-ARRAY"
   "SIMPLE-BIT-VECTOR"
   "SIMPLE-BIT-VECTOR-P"
   "SIMPLE-STRING"
   "SIMPLE-STRING-P"
   "SIMPLE-VECTOR"
   "SIMPLE-VECTOR-P"
   "SIN"
   "SINGLE-FLOAT"
   "SINGLE-FLOAT-EPSILON"
   "SINGLE-FLOAT-NEGATIVE-EPSILON"
   "SINH"
   "SIXTH"
   "SLEEP"
   "SOFTWARE-TYPE"
   "SOFTWARE-VERSION"
   "SOME"
   "SORT"
   "SPACE"
   "SPECIAL"
   "SPEED"
   "SQRT"
   "STABLE-SORT"
   "STANDARD-CHAR"
   "STANDARD-CHAR-P"
   "STEP"
   "STREAM"
   "STREAM-ELEMENT-TYPE"
   "STREAMP"
   "STRING"
   "STRING-CAPITALIZE"
   "STRING-CHAR"
   "STRING-CHAR-P"
   "STRING-DOWNCASE"
   "STRING-EQUAL"
   "STRING-GREATERP"
   "STRING-LEFT-TRIM"
   "STRING-LESSP"
   "STRING-NOT-EQUAL"
   "STRING-NOT-GREATERP"
   "STRING-NOT-LESSP"
   "STRING-RIGHT-TRIM"
   "STRING-TRIM"
   "STRING-UPCASE"
   "STRING/="
   "STRING<"
   "STRING<="
   "STRING="
   "STRING>"
   "STRING>="
   "STRINGP"
   "STRUCTURE"
   "SUBLIS"
   "SUBSEQ"
   "SUBSETP"
   "SUBST"
   "SUBST-IF"
   "SUBST-IF-NOT"
   "SUBSTITUTE"
   "SUBSTITUTE-IF"
   "SUBSTITUTE-IF-NOT"
   "SUBTYPEP"
   "SVREF"
   "SXHASH"
   "SYMBOL"
   "SYMBOL-FUNCTION"
   "SYMBOL-NAME"
   "SYMBOL-PACKAGE"
   "SYMBOL-PLIST"
   "SYMBOL-VALUE"
   "SYMBOLP"
   "T"
   "TAGBODY"
   "TAILP"
   "TAN"
   "TANH"
   "TENTH"
   "TERPRI"
   "THE"
   "THIRD"
   "THROW"
   "TIME"
   "TRACE"
   "TREE-EQUAL"
   "TRUENAME"
   "TRUNCATE"
   "TYPE"
   "TYPE-OF"
   "TYPECASE"
   "TYPEP"
   "UNEXPORT"
   "UNINTERN"
   "UNION"
   "UNLESS"
   "UNREAD-CHAR"
   "UNSIGNED-BYTE"
   "UNTRACE"
   "UNUSE-PACKAGE"
   "UNWIND-PROTECT"
   "UPPER-CASE-P"
   "USE-PACKAGE"
   "USER-HOMEDIR-PATHNAME"
   "VALUES"
   "VALUES-LIST"
   "VARIABLE"
   "VECTOR"
   "VECTOR-POP"
   "VECTOR-PUSH"
   "VECTOR-PUSH-EXTEND"
   "VECTORP"
   "WARN"
   "WHEN"
   "WITH-INPUT-FROM-STRING"
   "WITH-OPEN-FILE"
   "WITH-OPEN-STREAM"
   "WITH-OUTPUT-TO-STRING"
   "WRITE"
   "WRITE-BYTE"
   "WRITE-CHAR"
   "WRITE-LINE"
   "WRITE-STRING"
   "WRITE-TO-STRING"
   "Y-OR-N-P"
   "YES-OR-NO-P"
   "ZEROP"
   ))

(%resize-package (find-package "LISP"))

(defpackage "USER"
  (:use "LISP" "CCL"))  

(defconstant lisp:char-control-bit 0)
(defconstant lisp:char-meta-bit 0)
(defconstant lisp:char-super-bit 0)
(defconstant lisp:char-hyper-bit 0)
(defconstant lisp:char-bits-limit 1)
(defconstant lisp:char-font-limit 1)

(defun lisp:int-char (i)
  (cl:code-char i))

(defun lisp:char-bits (c)
  (require-type c 'character)
  0)

(defun lisp:char-font (c)
  (require-type c 'character)
  0)

(defun lisp:digit-char (weight &optional (radix 10) font)
  (when (and font (not (eql font 0)))
    (error "Non-zero ~S (~S) not supported" 'font font))
  (cl:digit-char weight radix))

; 'It is an error to give char-bit the name of a bit not supported by the
;   implementation'
(defun lisp:char-bit (char name)
  (declare (ignore char))
  (error "Unsupported character bit name ~S." name))

(defun lisp:set-char-bit (char name newvalue)
  (declare (ignore char newvalue))
  (error "Unsupported character bit name ~S." name))

(defun lisp:make-char (char &optional bits font)
  (flet ((non-supported (argname argval)
           (if (and argval (not (eql argval 0)))
             (error "Non-zero ~S argument (~S) not supported." argname argval))))
    (non-supported 'bits bits)
    (non-supported 'font font)
    (require-type char 'character)))

(defmacro define-setf-method (access-fn lambda-list &body body)
  `(define-setf-expander ,access-fn ,lambda-list ,@body))

(defun get-setf-method (form &optional environment)
  (get-setf-expansion-aux form environment nil))

(defun get-setf-method-multiple-value (form &optional environment)
  "Like Get-Setf-Method, but may return multiple new-value variables."
  (get-setf-expansion-aux form environment t))

; A tragic waste of precious silicon.
(define-setf-expander char-bit (place bit-name &environment env)
  (multiple-value-bind (dummies vals newval setter getter)
      (get-setf-expansion-aux place env nil)
    (let ((btemp (gensym))
	  (gnuval (gensym)))
      (values `(,@dummies ,btemp)
	      `(,@vals ,bit-name)
	      (list gnuval)
	      `(let ((,(car newval)
		      (set-char-bit ,getter ,btemp ,gnuval)))
		 ,setter
		 ,gnuval)
	      `(char-bit ,getter ,btemp)))))

(defun lisp:in-package (package-name &rest rest &key
                                     nicknames use internal-size external-size)
  (declare (ignore nicknames use internal-size external-size))
  (declare (dynamic-extent rest))
  (apply 'old-in-package package-name rest))

(defun lisp:functionp (x)
  (or (symbolp x)
      (and (consp x) (eq (ccl::%car x) 'lambda))
      (cl:functionp x)))

(setf (cl:find-class 'lisp:string-char) (cl:find-class 'cl:base-char)
      (symbol-function 'lisp:string-char-p) #'cl:characterp)

(dolist (sym '(lisp:make-package lisp:in-package lisp:shadow lisp:shadowing-import
               lisp:export lisp:unexport lisp:use-package lisp:unuse-package
               lisp:import))
  (unless (eq sym 'lisp:in-package)
    (setf (symbol-function sym)
          (symbol-function (find-symbol (symbol-name sym) "COMMON-LISP"))))
  (pushnew sym *fcomp-eval-always-functions*))

(provide :lisp-package)
