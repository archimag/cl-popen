;;;; process.lisp
;;;;
;;;; This file is part of the iolib.process library, released under MIT licence.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(defpackage :iolib.process
  (:use #:cl)
  (:nicknames #:iproc)
  (:export #:process-pid
           #:process-input
           #:process-output
           #:process-error
           #:process-close
           #:process-pool
           #:process-wait
           #:process-kill
           #:process-input-close
           #:create-process
           #:with-child-process))

(in-package :iolib.process)

(defclass process ()
  ((pid :initarg :pid :reader process-pid)
   (stdin :initarg :stdin :initform nil :reader process-input)
   (stdout :initarg :stdout :initform nil :reader process-output)
   (stderr :initarg :stderr :initform nil :reader process-error)))

(defun close-process-stream (stream)
  (when stream
    (close stream)
    (isys:close (or (iolib.streams:output-fd-of stream)
                    (iolib.streams:input-fd-of stream)))))

(defun process-close (process)
  "Close proccess streams and wait it terminated"
  (with-slots (pid stdin stdout stderr) process
    (close-process-stream stdin)
    (close-process-stream stdout)
    (close-process-stream stderr)
    (ignore-errors (isys:waitpid pid 0))))

(defconstant +WNOHANG+ 1)

(defun process-poll (process)
  "Check if child process has terminated. Returns returncode attribute or nil"
  (let ((status (isys:waitpid (process-pid process) +WNOHANG+)))
    (if (= status 0)
        nil
        status)))

(defun process-wait (process)
  "Wait for child process to terminate. Returns returncode attribute."
  (isys:waitpid (process-pid process) 0))

(defun process-kill (process signum)
  (isys:kill (process-pid process) signum))

(defun process-input-close (process)
  "Close input stream of the child process (send EOF code)"
  (close-process-stream (process-input process))
  (setf (slot-value process 'stdin) nil))

(defconstant +STDIN-FILENO+ 0)
(defconstant +STDOUT-FILENO+ 1)
(defconstant +STDERR-FILENO+ 2)

(defun create-process (cmd &key stdin stdout stderr union-stdout-stderr)
  "Open child process with command line cmd"
  (flet ((create-pipe (real)
           (if real
               (multiple-value-bind (p1 p2) (isys:pipe)
                 (vector p1 p2))))
         (pipe-rd (pipe)
           (aref pipe 0))
         (pipe-wr (pipe)
           (aref pipe 1)))
    (cffi:with-foreign-strings ((%arg0 "sh")
                                (%arg1 "-c") 
                                (%arg2 cmd))
      (cffi:with-foreign-object (%agrs :pointer 4)
        (setf (cffi:mem-aref %agrs :pointer 0) %arg0
              (cffi:mem-aref %agrs :pointer 1) %arg1
              (cffi:mem-aref %agrs :pointer 2) %arg2
              (cffi:mem-aref %agrs :pointer 3) (cffi:null-pointer))

        (let ((pin (create-pipe stdin))
              (pout (create-pipe (or stdout
                                     union-stdout-stderr)))
              (perr (create-pipe (and (not union-stdout-stderr)
                                      stderr)))
              (pid (isys:fork)))
          (case pid
            (0 (when stdin
                 (isys:close (pipe-wr pin))
                 (isys:dup2 (pipe-rd pin) +STDIN-FILENO+)
                 (isys:close (pipe-rd pin)))

               (cond
                 (union-stdout-stderr
                  (isys:close (pipe-rd pout))
                  (isys:dup2 (pipe-wr pout) +STDOUT-FILENO+)
                  (isys:dup2 (pipe-wr pout) +STDERR-FILENO+)
                  (isys:close (pipe-wr pout)))

                 (t (when stdout
                      (isys:close (pipe-rd pout))
                      (isys:dup2 (pipe-wr pout) +STDOUT-FILENO+)
                      (isys:close (pipe-wr pout)))
                    (when stderr
                      (isys:close (pipe-rd perr))
                      (isys:dup2 (pipe-wr perr) +STDERR-FILENO+)
                      (isys:close (pipe-wr perr)))))
               
               (isys:execv "/bin/sh" %agrs))
            (otherwise
             (when pin
               (isys:close (pipe-rd pin)))
             (when pout
               (isys:close (pipe-wr pout)))
             (when perr
               (isys:close (pipe-wr perr)))

             (make-instance 'process
                            :pid pid
                            :stdin (if pin
                                       (make-instance 'iolib.streams:dual-channel-gray-stream
                                                      :output-fd (pipe-wr pin)))
                            :stdout (if pout
                                        (make-instance 'iolib.streams:dual-channel-gray-stream
                                                       :input-fd (pipe-rd pout)))
                            :stderr (if perr
                                        (make-instance 'iolib.streams:dual-channel-gray-stream
                                                       :input-fd (pipe-rd perr)))))))))))
      
(defmacro with-child-process ((process cmd &key stdin stdout stderr union-stdout-stderr) &body body)
  `(let ((,process (create-process ,cmd
                                   :stdin ,stdin
                                   :stdout ,stdout
                                   :stderr ,stderr
                                   :union-stdout-stderr ,union-stdout-stderr)))
     (unwind-protect
          (progn ,@body)
       (process-close ,process))))
