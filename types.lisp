;;; This file was automatically generated by SWIG (http://www.swig.org).
;;; Version 3.0.12
;;;
;;; Do not make changes to this file unless you know what you are doing--modify
;;; the SWIG interface file instead.

(in-package :jack-transport)

;;;SWIG wrapper code starts here

(cl:defmacro defanonenum (cl:&body enums)
   "Converts anonymous enums to defconstants."
  `(cl:progn ,@(cl:loop for value in enums
                        for index = 0 then (cl:1+ index)
                        when (cl:listp value) do (cl:setf index (cl:second value)
                                                          value (cl:first value))
                        collect `(cl:defconstant ,value ,index))))

(cl:eval-when (:compile-toplevel :load-toplevel)
  (cl:unless (cl:fboundp 'swig-lispify)
    (cl:defun swig-lispify (name flag cl:&optional (package cl:*package*))
      (cl:labels ((helper (lst last rest cl:&aux (c (cl:car lst)))
                    (cl:cond
                      ((cl:null lst)
                       rest)
                      ((cl:upper-case-p c)
                       (helper (cl:cdr lst) 'upper
                               (cl:case last
                                 ((lower digit) (cl:list* c #\- rest))
                                 (cl:t (cl:cons c rest)))))
                      ((cl:lower-case-p c)
                       (helper (cl:cdr lst) 'lower (cl:cons (cl:char-upcase c) rest)))
                      ((cl:digit-char-p c)
                       (helper (cl:cdr lst) 'digit 
                               (cl:case last
                                 ((upper lower) (cl:list* c #\- rest))
                                 (cl:t (cl:cons c rest)))))
                      ((cl:char-equal c #\_)
                       (helper (cl:cdr lst) '_ (cl:cons #\- rest)))
                      (cl:t
                       (cl:error "Invalid character: ~A" c)))))
        (cl:let ((fix (cl:case flag
                        ((constant enumvalue) "+")
                        (variable "*")
                        (cl:t ""))))
          (cl:intern
           (cl:concatenate
            'cl:string
            fix
            (cl:nreverse (helper (cl:concatenate 'cl:list name) cl:nil cl:nil))
            fix)
           package))))))

;;;SWIG wrapper code ends here

(defctype jack-nframes-t :int32)
(defctype jack-uuid-t :uint64)
(defctype jack-shmsize-t :int32)
(defctype jack-unique-t :uint64)

(cl:defconstant JACK-MAX-FRAMES 4294967295)

(defctype jack-time-t :uint64)

(cl:defconstant JACK-LOAD-INIT-LIMIT 1024)

(defctype jack-intclient-t :uint64)

(defctype jack-port-id-t :uint32)
(defctype jack-port-type-id-t :uint64)


(cffi:defcenum JackOptions
	(:JackNullOption #.#x00)
	(:JackNoStartServer #.#x01)
	(:JackUseExactName #.#x02)
	(:JackServerName #.#x04)
	(:JackLoadName #.#x08)
	(:JackLoadInit #.#x10)
	(:JackSessionID #.#x20))

(cffi:defcenum JackStatus
	(:JackFailure #.#x01)
	(:JackInvalidOption #.#x02)
	(:JackNameNotUnique #.#x04)
	(:JackServerStarted #.#x08)
	(:JackServerFailed #.#x10)
	(:JackServerError #.#x20)
	(:JackNoSuchClient #.#x40)
	(:JackLoadFailure #.#x80)
	(:JackInitFailure #.#x100)
	(:JackShmFailure #.#x200)
	(:JackVersionError #.#x400)
	(:JackBackendError #.#x800)
	(:JackClientZombie #.#x1000))

(cffi:defcenum JackLatencyCallbackMode
	:JackCaptureLatency
	:JackPlaybackLatency)

(cffi:defcstruct -jack-latency-range
	(min :pointer)
	(max :pointer))

(unless (boundp 'JACK-DEFAULT-AUDIO-TYPE)
  (cl:defconstant JACK-DEFAULT-AUDIO-TYPE "32 bit float mono audio"))
(unless (boundp 'JACK-DEFAULT-AUDIO-TYPE)
  (cl:defconstant JACK-DEFAULT-MIDI-TYPE "8 bit raw midi"))


(cffi:defcenum JackPortFlags
	(:JackPortIsInput #.#x1)
	(:JackPortIsOutput #.#x2)
	(:JackPortIsPhysical #.#x4)
	(:JackPortCanMonitor #.#x8)
	(:JackPortIsTerminal #.#x10))

(cffi:defcenum jack-transport-state-t
	(:JackTransportStopped #.0)
	(:JackTransportRolling #.1)
	(:JackTransportLooping #.2)
	(:JackTransportStarting #.3)
	(:JackTransportNetStarting #.4))

(cffi:defcenum jack-position-bits-t
	(:JackPositionBBT #.#x10)
	(:JackPositionTimecode #.#x20)
	(:JackBBTFrameOffset #.#x40)
	(:JackAudioVideoRatio #.#x80)
	(:JackVideoFrameOffset #.#x100)
	(:JackTickDouble #.#x200))

(cffi:defcstruct jack-position
	(unique-1 jack-unique-t)
	(usecs jack-time-t)
	(frame-rate jack-nframes-t)
	(frame jack-nframes-t)
	(valid jack-position-bits-t)
	(bar :int32)
	(beat :int32)
	(tick :int32)
	(bar-start-tick :double)
	(beats-per-bar :float)
	(beat-type :float)
	(ticks-per-beat :double)
	(beats-per-minute :double)
	(frame-time :double)
	(next-time :double)
	(bbt-offset jack-nframes-t)
	(audio-frames-per-video-frame :float)
	(video-offset jack-nframes-t)
	(tick-double :double)
	(padding :pointer :count 5)
  (unique-2 :pointer))

(defctype jack-position-t (:struct jack-position))


(cffi:defcenum jack-transport-bits-t
	(:JackTransportState #.#x1)
	(:JackTransportPosition #.#x2)
	(:JackTransportLoop #.#x4)
	(:JackTransportSMPTE #.#x8)
	(:JackTransportBBT #.#x10))

(cffi:defcstruct jack-transport-info-t
	(frame-rate :pointer)
	(usecs :pointer)
	(valid jack-transport-bits-t)
	(transport-state jack-transport-state-t)
	(frame :pointer)
	(loop-start :pointer)
	(loop-end :pointer)
	(smpte-offset :long)
	(smpte-frame-rate :float)
	(bar :int)
	(beat :int)
	(tick :int)
	(bar-start-tick :double)
	(beats-per-bar :float)
	(beat-type :float)
	(ticks-per-beat :double)
	(beats-per-minute :double))


