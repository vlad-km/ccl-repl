;;; -*- mode:lisp; coding:utf-8 -*-

;;; This file is part of the ccl-repl package
;;; Copyright © 2017,2018 Vladimir Mezentsev


;;;
;;; trivial ccl repl for Moren environment
;;;
;;; command
;;;         :ps - processes status
;;;         :ping ccl
;;;         :exit terminate repl
(defun repl ()
    (prog ((input)
           (state))
     rdr
       (format t "HOST: ~a>" (package-name *package*))
       (handler-case
           (progn
               (setq input (read))
               (if state
                   (case state
                     (path (go spath))))
               (if (symbolp input)
                   (cond ((eql input :exit) (go exit))
                         ((eql input :ps) (go pstate))
                         ((eql input :ping) (go ping))
                         ((eql input :pwd) (go pwd-state))
                         ((eql input :cd)
                          (setq state 'path)
                          (go rdr)) ))
               (let ((outbound (multiple-value-list (eval input))))
                   (dolist (it outbound)
                       (princ it )
                       (terpri)
                       (force-output))))
         (error (emsg)
             (format t "ERROR: ~s~%" input)
             (format t "~a~%" emsg)))
       (go rdr)
     pwd-state
       (princ (ccl:current-directory))
       (terpri)
       (force-output)
       (go rdr)
     ping
       (princ :pong)
       (terpri)
       (force-output)
       (go rdr)
     spath
       (if (stringp input)
           (setf (ccl:current-directory) input)
           (progn
               (format t "cd argument must be a string")
               (terpri)
               (force-output)))
       (setq state nil)
       (go rdr)
     pstate
       (ps)
       (go rdr)
     exit
       (princ 'bye)
       (terpri)
       (force-output)
       (return-from repl t)))



;;; EOF
