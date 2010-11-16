;;;; cl-popen.asd
;;;;
;;;; This file is part of the cl-popen library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(defsystem cl-popen
  :maintainer "Moskvitin Andrey <archimag@gmail.com>"
  :depends-on (#:iolib.streams)
  :components ((:file "process")))

(defmethod perform ((o test-op) (c (eql (find-system 'cl-popen))))
  (operate 'load-op 'cl-popen-test)
  (operate 'test-op 'cl-popen-test :force t))

(defsystem cl-popen-test
  :depends-on (#:cl-popen #:lift)
  :components ((:file "t")))

(defmethod perform ((o test-op) (c (eql (find-system 'cl-popen-test))))
  (operate 'load-op 'cl-popen-test )
  (let* ((test-results (funcall (read-from-string "cl-popen.test:run-cl-popen-tests")))
         (errors (funcall (read-from-string "lift:errors")  test-results))
         (failures (funcall (read-from-string "lift:failures") test-results)))
    (if (or errors failures)
        (error "test-op failed: ~A"
               (concatenate 'list errors failures))
        (print test-results))))