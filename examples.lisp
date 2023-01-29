;;; 
;;; examples.lisp
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

(in-package :cl-user)

;;; usage examples

;;; connect to jack:
(jack-transport:connect)

;;; set responders for the transport state change situations:

(jack-transport:set-transport-responder
 :start
 (lambda ()
   (format t "~&start!")))

(jack-transport:set-transport-responder
 :stop
 (lambda ()
   (format t "~&stop!")
   ))

;;; sync is called whenever the transport position has been changed.
;;; Be aware that sync is not only called after relocation of
;;; jacktransport, but also every time when starting, so it might be a
;;; good idea to check for the internal position of the app and jack's
;;; position when starting to avoid unnecessary multiple relocations.

(jack-transport:set-transport-responder
 :sync
 (lambda ()
   (format t "~&syncing to ~,3fs ..." (jack-transport:get-position))
   (sleep 0) ;;; do something to sync up
   (format t "done!")))

;;; there are some utility functions:

(jack-transport:locate 10)

(jack-transport:get-position)

(jack-transport:start)

(jack-transport:stop)

(jack-transport:transport-state)

(jack-transport:get-frame-rate)

;;; any other function of the jack API can also be called directly
;;; using the cffi calling conventions (e.g. replace underscore
;;; characters with hyphens). The current jack client is stored in the
;;; var jack-transport::*transport-client*.

;;; Example setting the sync timeout:

(let* ((timeout-ptr (cffi:foreign-alloc :uint64)))
  (setf (cffi:mem-aref timeout-ptr :uint64 0) 1)
  (let ((result
          (jack-transport::jack-set-sync-timeout
           jack-transport::*transport-client*
           timeout-ptr)))
    (cffi:foreign-free timeout-ptr)
    result))
