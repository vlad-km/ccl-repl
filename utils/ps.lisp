;;;; -*- mode: lisp; coding: utf-8 -*-

;;; This file is part of the ccl-repl package
;;; Copyright © 2017,2018 Vladimir Mezentsev
;;;


;;; trivial processes status command for
;;; host ccl repl
;;;
;;; (ps) / :ps
;;;
;;; =>
;;;
;;; System Clozure Common Lisp Version 1.11-r16635  (WindowsX8632)  load time 0h 0 m 45.646sec
;;;  listener                                 Active                         0000000837  00:00:44.809
;;;  Initial                                  Reset                          0000000130  00:00:45.516
;;;
(defparameter *machine-instance* (intern (concatenate 'string
                                                      (lisp-implementation-type) " "
                                                      (lisp-implementation-version) " ")))

(defvar *saved-real-time* 0)


(defun %decode-load-time ()
    (let ((min 0.0))
        (labels ((load-time-sec ()
                     (/ (cl:get-internal-real-time)
                        cl:internal-time-units-per-second))
                 (load-time-hour ()
                     (ftruncate (/ (load-time-sec) 3600.0)))
                 (decode-load-time-sec (m)
                     (multiple-value-bind (minute percent)(ftruncate m)
                         (declare (ignore minute))
                         (* 60.0 percent))))
            (multiple-value-bind (hour percent) (load-time-hour)
                (setf min (* 60.0 percent))
                (values (truncate hour) (truncate min) (decode-load-time-sec min))))))


(defun forced-format (&rest args)
    (declare (dynamic-extent args))
    (apply #'format t args)
    (force-output))


(defun %%save-current-real-time ()
    (declare (optimize (speed 3) (safety 0) (debug 0)))
    (setf *saved-real-time* (cl:get-internal-real-time)))

(defun %%load-time-hour (creation-time)
    (declare (optimize (speed 3) (safety 0) (debug 0)))
    (ftruncate (/ (/ (- *saved-real-time*  creation-time)
                     cl:internal-time-units-per-second)
                  3600.0)))

(defun %%decode-load-time-sec (m)
    (declare (optimize (speed 3) (safety 0) (debug 0)))
    (multiple-value-bind (minute percent) (ftruncate m)
        (declare (ignore minute))
        (* 60.0 percent)))

(defun %%decode-process-load-time (creation-time)
    (declare (optimize (speed 3) (safety 0) (debug 0)))
    (let ((min 0.0))
        (multiple-value-bind (hour percent) (%%load-time-hour creation-time)
            (setf min (* 60.0 percent))
            (list (truncate hour) (truncate min) (%%decode-load-time-sec min)))))

(defun %%thread-status (x)
    ;;  (declare (optimize (speed 3) (safety 0) (debug 0)))
    (let ((status (format nil "~a" (ccl:process-whostate x))))
        (cond ((> (length status) 30)
               (setf status (subseq status 0 29))))
        status))

(defun ps ()
    (declare (optimize (speed 3) (safety 0) (debug 0)))
    (multiple-value-bind (hours min sec)  (%decode-load-time)
        (forced-format "System ~a load time ~ah ~2am ~asec~%"
                       *machine-instance*
                       hours min sec))
    (%%save-current-real-time)
    (mapcar #'(lambda (x)
                  (let* ((creation (ccl::process-creation-time x))
                         (ff (%%decode-process-load-time creation)))
                      (format t " ~40a ~30a ~10,'0d  ~2,'0d:~2,'0d:~03$~%"
                              (ccl:process-name x) (%%thread-status x)
                              creation (first ff)(second ff) (third ff))))
            (ccl:all-processes)) (values))

;;; EOF
