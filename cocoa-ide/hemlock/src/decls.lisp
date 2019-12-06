(in-package :hemlock-internals)

;;; Use #.*fast* for optimizations.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *fast*
    '(declare (optimize speed)))

  (defparameter *fast*
    '(declare)))


;; Since the declaim form for functions looks clumsy and is
;; syntax-wise different from defun, we define us a new declfun, which
;; fixes this.

(defmacro declfun (name lambda-list)
  `(declaim (ftype (function
                    ,(let ((q lambda-list)
                           res)
                          (do () ((or (null q)
                                      (member (car q) '(&optional &rest &key))))
                            (push 't res)
                            (pop q))
                          (when (eq (car q) '&optional)
                            (push '&optional res)
                            (pop q)
                            (do () ((or (null q)
                                        (member (car q) '(&rest &key))))
                              (push 't res)))
                          (when (eq (car q) '&rest)
                            (push '&rest res)
                            (pop q)
                            (push 't res)
                            (pop q))
                          (when (eq (car q) '&key)
                            (push '&key res)
                            (pop q)
                            (do () ((or (null q)
                                        (member (car q) '(&allow-other-keys))))
                              (push (list (intern (string (if (consp (car q))
                                                              (if (consp (caar q))
                                                                  (caaar q)
                                                                  (caar q))
                                                              (car q)))
                                                  :keyword)
                                          't)
                                    res)
                              (pop q)))
                          (when (eq (car q) '&allow-other-keys)
                            (push '&allow-other-keys res)
                            (pop q))
                          (reverse res))
                    t)
             ,name)))

;;; Some special variables are forward-referenced, and we don't even
;;; need to invent a new language to advise the compiler of that ...
(declaim (special *mode-names* *current-buffer*
		  *the-sentinel*
		  *in-the-editor* *buffer-list* *things-to-do-once*
		  *gc-notify-before* *gc-notify-after*
                  *key-event-history*))
