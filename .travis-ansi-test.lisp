(setf *default-pathname-defaults* (merge-pathnames "ansi-test/" (user-homedir-pathname)))
(load "gclload1.lsp")
(load "gclload2.lsp")
(in-package :rt)
(disable-note :ansi-spec-problem)
(disable-note :result-type-element-type-by-subtype)
(setf *default-pathname-defaults* (truename #P"sandbox/"))
(in-package :cl-test)
(rt:do-tests)
(ccl:quit (if *failed-tests* 1 0))
