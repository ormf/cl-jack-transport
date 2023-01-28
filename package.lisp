;;;; package.lisp

(defpackage #:jack-transport
  (:use #:cl #:cffi))

(in-package :jack-transport)

(define-foreign-library libjack
    (:darwin (:or "libjack.0.dylib" "libjack.dylib"))
    (:unix (:or "libjack.so.0" "libjack.so"))
    (t (:default "libjack")))
   
(use-foreign-library libjack)


;;; (c-include "jack.h")

