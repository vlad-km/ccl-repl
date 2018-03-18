;;; -*- mode:lisp; coding:utf-8 -*-

;;; This file is part of the ccl-repl package
;;; Copyright © 2017,2018 Vladimir Mezentsev
;;;


(in-package :cl-user)

(defpackage :ccl
  (:use #:cl)
  (:export #:options #:connect #:drop #:kb-setup #:send  #:repl ))

(in-package :ccl)
(export '(jscl::lisp-to-js jscl::concat))

;;; ccl path
(defparameter *lisp* "wx86cl.exe")

;;; ccl image path
(defparameter *lisp-image*
  (vector (lisp-to-js "-I")
          (lisp-to-js "wx86cl.image")))

;;; wx86cl arguments
;;; any ccl legal command line arguments
(defun options (&rest items)
    (apply 'vector (mapcar (lambda (x) (lisp-to-js x)) items)))


;;; spawn ccl process and start session
;;;
;;; example:
;;; (ccl:connect)
;;; =>
;;; Welcome to Clozure Common Lisp Version 1.11-r16635  (WindowsX8632)!
;;;
;;; CCL is developed and maintained by Clozure Associates. For more information
;;; about CCL visit http://ccl.clozure.com.  To enquire about Clozure's Common Lisp
;;; consulting services e-mail info@clozure.com or visit http://www.clozure.com.
;;;
;;;?
;;;
;;; (ccl:kb-setup)
;;; (ccl:repl)
;;; or
;;; (repl)<ctrl-L>
;;;
;;;
(defun connect (&key (path *lisp*) (args *lisp-image*))
    (setf #j:child_process (require "child_process"))
    (setf #j:ccl (jso:mcall (#j:child_process "spawn")
                            path
                            args))
    (#j:ccl:stdout:on "data"
                      (lambda (data &optional a b c)
                          (receive data)))

    (#j:ccl:stdout:on "readable"
                      (lambda (&optional data a b c d)
                          (#j:console:log "readable" a b c d)))

    (#j:ccl:stdout:on "close"
                      (lambda (&optional data a b c d)
                          (#j:console:log "close" data a b c d)
                          (format t "~%CCL CLOSE~%") ))

    (#j:ccl:stdout:on "end"
                      (lambda (&optional data a b c d)
                          (#j:console:log "end" data a b c d)))

    (#j:ccl:stderr:on "data"
                      (lambda (&optional data a b c d)
                          (#j:console:log "error" (jso:mcall (data "toString")))))
    (values))


;;; kill ccl connection
(defun drop ()
    (#j:ccl:kill)
    (values))


;;; send data to ccl process
;;; data will be treminated (code-char #\newline)
;;; see (read-send) for detail
(defun send (data)
    (#j:ccl:stdin:write data)
    (values))

;;; (send (terminate data))
(defun terminate (data)
    (concat data (code-char #\newline)))

;;; data received from ccl process
(defun receive (data)
    (format t "~a" (jso:mcall (data "toString"))))


;;; moren keyboard shortcut
(defun kb-setup (&optional (letter "L"))
    (#j:jqconsole:RegisterShortcut
     letter
     (lambda ()
         (read-send)
         (values)))
    (format t "Use Ctrl-~a key for send text to ccl process~%" letter))


;;; internal keyboard input driver
;;; read from jqconsole - send to ccl stdinput
(defun read-send ()
    (let* ((input (#j:jqconsole:GetPromptText)))
        (#j:jqconsole:ClearPromptText)
        (format t "~a~%" input)
        ;; (send (terminate input))
        (send (concat input (code-char #\newline)))
        (values)))


;;; invocation ccl repl
;;; - send src lisp text
;;; - run
(defun repl ()
    (send "(repl)")
    (values))


(in-package :cl-user)







;;; EOF
