;;; 
;;; callbacks.lisp
;;;
;;; callback mechanism for jack-transport.
;;;
;;; use #'set-transport-responder with the keywords :start, :stop or
;;; :sync to set responder functions of no arguments when the
;;; transport state is changed. These functions will be called in
;;; their own thread to avoid blocking of jack's rt-thread.
;;;
;;; **********************************************************************
;;; Copyright (c) 2023 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>
;;;
;;; Revision history: See git repository.
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the Gnu Public License, version 2 or
;;; later. See https://www.gnu.org/licenses/gpl-2.0.html for the text
;;; of this agreement.
;;; 
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;; GNU General Public License for more details.
;;;
;;; **********************************************************************

(in-package :jack-transport)

(defvar *transport-states* #(:stopped :rolling :looping :starting :net-starting))

(defvar *sync-signal* (bt:make-condition-variable))
(defvar *start-signal* (bt:make-condition-variable))
(defvar *stop-signal* (bt:make-condition-variable))
(defvar *transport-responder-fns*
  `(:sync ,(lambda ())
    :start ,(lambda ())
    :stop ,(lambda ())))

(defvar *sync-state* :idle)
(defvar *sync-thread* nil)
(defvar *transport-thread* nil)

(defun set-transport-responder (responder fn)
  (setf (getf *transport-responder-fns* responder) fn))

(defun signal-sync ()
  "signal the sync thread to call the sync responder."
  (bt:condition-notify *sync-signal*))

(defun signal-start ()
  "signal the sync thread to call the start responder."
  (bt:condition-notify *start-signal*))

(defun signal-stop ()
  "signal the sync thread to call the stop responder."
  (bt:condition-notify *stop-signal*))

(defun make-sync-thread ()
  (let ((lock (bt:make-lock)))
    (setf *sync-thread*
          (bt:make-thread
           (lambda () (loop (bt:with-lock-held (lock)
                         (bt:condition-wait *sync-signal* lock)
                         (funcall (getf *transport-responder-fns* :sync))
                         (setf *sync-state* :synced))))))))

(defun make-transport-thread ()
  (let ((lock (bt:make-lock)))
    (setf *transport-thread*
          (bt:make-thread
           (lambda () (loop (bt:with-lock-held (lock)
                         (bt:condition-wait *start-signal* lock)
                         (funcall (getf *transport-responder-fns* :start))
                         (bt:condition-wait *stop-signal* lock)
                         (funcall (getf *transport-responder-fns* :stop)))))))))

(defun destroy-all-threads ()
  (when *sync-thread*
    (bt:destroy-thread *sync-thread*)
    (setf *sync-thread* nil))
  (when *transport-thread*
    (bt:destroy-thread *transport-thread*)
    (setf *transport-thread* nil)))

(defcallback jacktransport-shutdown-callback :void
    ((arg (:pointer :void)))
  (declare (ignore arg))
  (disconnect))

(defcallback jacktransport-sync-callback :int
    ((state jack-transport-state-t)
     (pos (:pointer (:struct jack-position)))
     (arg (:pointer :void)))
  (declare (ignore state pos arg))
  (case *sync-state*
    (:idle
     (setf *sync-state* :syncing)
     (signal-sync)))
  (if (eql *sync-state* :synced)
      (progn
        (setf *sync-state* :idle)
        1)
      0))

(let ((old-state -1))
  (defcallback jacktransport-process-callback :int
      ((nframes jack-nframes-t)
       (arg (:pointer :void)))
    (declare (ignore nframes arg))
    (let ((state (jack-transport-query *transport-client* *transport-position*)))
      (when (/= state old-state)
        (case (aref *transport-states* state)
          (:rolling (signal-start))
          (:stopped (signal-stop))
          (:otherwise (format t "~&~S" (aref *transport-states* state))))
        (setf old-state state))
      0)))

