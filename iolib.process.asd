;;;; iolib.process.asd
;;;;
;;;; This file is part of the iolib.process library, released under MIT licence.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>


(defpackage #:iolib.process-system
  (:use #:cl #:asdf))

(in-package #:iolib.process-system)

(defsystem iolib.process
  :description "Gray streams."
  :maintainer "Moskvitin Andrey <archimag@gmail.com>"
  :licence "MIT"
  :depends-on (#:iolib.streams)
  :components ((:file "process")))

(defmethod perform ((o test-op) (c (eql (find-system 'iolib.process))))
  (operate 'load-op 'iolib.process-test)
  (operate 'test-op 'iolib.process-test :force t))

(defsystem iolib.process-test
  :depends-on (#:iolib.process #:lift)
  :components ((:file "t")))

(defmethod perform ((o test-op) (c (eql (find-system 'iolib.process-test))))
  (operate 'load-op 'iolib.process-test )
  (let* ((test-results (funcall (read-from-string "iolib.process.test:run-iolib.process-tests")))
         (errors (funcall (read-from-string "lift:errors")  test-results))
         (failures (funcall (read-from-string "lift:failures") test-results)))
    (if (or errors failures)
        (error "test-op failed: ~A"
               (concatenate 'list errors failures))
        (print test-results))))