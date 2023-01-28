;;; 
;;; jack-transport.lisp
;;;
;;; **********************************************************************
;;; Copyright (c) 2022 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>
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

(setf *print-case* :downcase)

(defvar *transport-client* nil) ;;; package global handle for the
                                ;;; client. This should never be
                                ;;; modified directly.
(defvar *transport-position* nil) ;;; package global handle for the
                                  ;;; transport-position
                                  ;;; c-struct. This should never be
                                  ;;; modified directly. The c-struct
                                  ;;; gets created with
                                  ;;; jacktransport-connect and freed
                                  ;;; with jacktransport-disconnect.

(defmacro ensure-jacktransport-connected (&body body)
  `(if *transport-client* (progn ,@body)
       (warn "jacktransport: not connected. Connect first with (jacktransport-connect).")))

(defmacro ensure-jacktransport-disconnected (&body body)
  `(if (not *transport-client*) (progn ,@body)
       (warn "jacktransport: already connected. Disconnect first with (jacktransport-disconnect).")))

(defun jacktransport-connect ()
  "connect jacktransport to jack."
  (ensure-jacktransport-disconnected
    (format t "~&connecting jacktransport...")
    (cffi:with-foreign-pointer (status 1)
      (let ((result (jack-client-open "cl-transport" :JackNullOption status)))  
        (if (zerop (mem-aref status :int))
            (progn
              (setf *transport-position* (foreign-alloc '(:struct jack-position)))
              (setf *transport-client* result)
              (format t "done.")
              (values))
            (warn "couldn't create a jack client (~d)!" (mem-aref status :int)))))))

(defun jacktransport-disconnect ()
  "disconnect jacktransport from jack."
  (ensure-jacktransport-connected
    (format t "~&disconnecting jacktransport...")
    (jack-client-close *transport-client*)
    (if *transport-position* (foreign-free *transport-position*))
    (setf *transport-client* nil)
    (setf *transport-position* nil)
    (format t "done.")
    (values)))

(defun jack-get-frame-rate ()
  (ensure-jacktransport-connected
    (jack-transport-query *transport-client* *transport-position*)
    (foreign-slot-value *transport-position* '(:struct jack-position) 'frame-rate)))

(defun jacktransport-locate (time)
  "locate jacktransport to time in secs."
  (ensure-jacktransport-connected
    (jack-transport-locate *transport-client* (* time (jack-get-frame-rate)))
    time))

(defun jacktransport-get-position ()
  "get jacktransport position in secs."
  (ensure-jacktransport-connected
    (let ((frame-rate (foreign-slot-value *transport-position* '(:struct jack-position) 'frame-rate)))
      (/ (jack-get-current-transport-frame *transport-client*) frame-rate))))

;;; (jacktransport-connect)
;;; (jacktransport-locate 30)
;;; (jacktransport-get-position)
;;; (jacktransport-disconnect)
