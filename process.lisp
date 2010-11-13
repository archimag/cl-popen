;;;; process.lisp
;;;;
;;;; This file is part of the iolib.process library, released under MIT licence.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(defpackage #:popen
  (:use #:cl)
  (:nicknames #:iproc)
  (:export #:process-pid
           #:process-input
           #:process-output
           #:process-error
           #:process-close
           #:process-poll
           #:process-wait
           #:process-kill
           #:create-process
           #:popen
           #:with-popen
           #:with-popen2
           #:with-popen3
           #:with-popen4))

(in-package #:popen)

(defclass process ()
  ((pid :initarg :pid :reader process-pid)
   (stdin :initarg :stdin :initform nil :reader process-input)
   (stdout :initarg :stdout :initform nil :reader process-output)
   (stderr :initarg :stderr :initform nil :reader process-error)))

(defun process-close (process)
  "Close proccess streams and wait it terminated"
  (flet ((safe-close (stream)
           (when (and stream (iolib.streams:fd-of stream))
             (close stream :abort t))))
  (with-slots (pid stdin stdout stderr) process
    (safe-close stdin)
    (safe-close stdout)
    (safe-close stderr)
    (ignore-errors (isys:waitpid pid 0)))))

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
            (0 
               (when stdin
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
                                                      :fd (pipe-wr pin)))
                            :stdout (if pout
                                        (make-instance 'iolib.streams:dual-channel-gray-stream
                                                       :fd (pipe-rd pout)))
                            :stderr (if perr
                                        (make-instance 'iolib.streams:dual-channel-gray-stream
                                                       :fd (pipe-rd perr)))))))))))  
      
(defmacro with-popen ((cmd process &key stdin stdout stderr union-stdout-stderr) &body body)
  `(let ((,process (create-process ,cmd
                                   :stdin ,stdin
                                   :stdout ,stdout
                                   :stderr ,stderr
                                   :union-stdout-stderr ,union-stdout-stderr)))
     (unwind-protect
          (progn ,@body)
       (process-close ,process))))

(defun popen (cmd &key (buffer-size 4096))
  (with-popen (cmd process :stdout t)
    (let ((*print-pretty* nil)
          (pout (process-output process))
          (buffer (make-array buffer-size :element-type 'character)))
      (with-output-to-string (out)
        (loop
           :for bytes-read = (read-sequence buffer pout)
           :do (write-sequence buffer out :start 0 :end bytes-read)
           :while (= bytes-read buffer-size))))))



(defmacro with-popen2 ((cmd process stdin stdout) &body body)
  `(with-popen (,cmd ,process :stdin t :stdout t)
     (let ((,stdin (process-input ,process))
           (,stdout (process-output ,process)))
       ,@body)))

(defmacro with-popen3 ((cmd process stdin stdout stderr) &body body)
  `(with-popen (,cmd ,process :stdin t :stdout t :stderr t)
     (let ((,stdin (process-input ,process))
           (,stdout (process-output ,process))
           (,stderr (process-error ,process)))
       ,@body)))

(defmacro with-popen4 ((cmd process stdin stdout-and-stderr) &body body)
  `(with-popen (,cmd ,process :stdin t :union-stdout-stderr t)
     (let ((,stdin (process-input ,process))
           (,stdout-and-stderr (process-output ,process)))
       ,@body)))

  