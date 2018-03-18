;;; -*- mode:lisp; coding:utf-8 -*-

;;; This file is part of the ccl-repl package
;;; jscl host side compiler interface for lores:sdf
;;; Copyright © 2017,2018 Vladimir Mezentsev
;;;


(defpackage #:hsdf
  (:use #:cl)
  (:export #:sys  #:sys-name #:sys-path
           #:sys-components #:module-name
           #:create
           #:add-path
           #:add-component
           #:compile-it))

(in-package #:hsdf)

(defclass sys ()
  ((name :accessor sys-name :initform nil :initarg :name)
   (path :accessor sys-path :initform nil :initarg :path)
   (components :accessor sys-components :initform nil)
   (module :accessor module-name :initform nil :initarg :module)))



(defun create (&key name path module)
    (make-instance 'sys :name name :path path :module module))


(defmethod add-path ((el sys) (path string))
    (setf (sys-path el) path))

(defmethod verify-component ((el sys) name)
    (let ((full name)
          (result))
        (if (sys-path el)
            (setf full (concatenate 'string (sys-path el) "/" name)))
        (setq result (directory full))
        (unless result
            (error "component ~s not exists" name))
        result))


(defmethod add-component ((el sys) name)
    (setf (sys-components el)
          (append (sys-components el) (verify-component el name))))


(defmethod compile-it ((el sys) &optional name)
    (handler-case
        (progn
            (jscl::compile-application (sys-components el) (if name name (module-name el))))
      (error (msg)
          (format t "It is impossible to compile ~a~%" (sys-name el))
          (format t "~a" msg))))

(in-package :cl-user)





;;; EOF
