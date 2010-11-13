;;;; t.lisp
;;;;
;;;; This file is part of the cl-popen library, released under MIT licence.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(defpackage #:popen.test
  (:use #:cl #:lift #:popen)
  (:export #:run-popen-tests))

(in-package :popen.test)

(defun run-popen-tests ()
  (run-tests :suite 'popen-test
             :report-pathname nil))

(deftestsuite popen-test () ())

(addtest (popen-test)
  simple1
  (ensure-same "Hello world"
               (with-popen ("python -c \"print 'Hello world'\"" python :stdout t)
                 (read-line (process-output python)))))

(addtest (popen-test)
  popen2-1
  (ensure-same "Hello Common Lisp"
               (with-popen2 ("python" python pin pout)
                 (write-line "print 'Hello Common Lisp'" pin)
                 (close pin)
                 (read-line pout))))

(addtest (popen-test)
  popen2-2
  (ensure-same "Hello Common Lisp"
               (with-popen2 ("cat" cat pin pout)
                 (write-line "Hello Common Lisp" pin)
                 (close pin)
                 (read-line pout))))

(addtest (popen-test)
  popen3
  (ensure-same '("Common Lisp is good"
                 "Python is bad")
               (with-popen3 ("python" python pin pout perr)
                 (write-line "import sys" pin)
                 (write-line "print 'Common Lisp is good'" pin)
                 (write-line "print >> sys.stderr, 'Python is bad'" pin)
                 (close pin)
                 (list (read-line pout)
                       (read-line perr)))))

(addtest (popen-test)
  popen4
  (ensure-same '("Hello" "Hello")
               (with-popen4 ("python" python pin pout)
                 (write-line "import sys" pin)
                 (write-line "print 'Hello'" pin)
                 (write-line "print >> sys.stderr, 'Hello'" pin)
                 (close pin)
                 (list (read-line pout)
                       (read-line pout)))))

(addtest (popen-test)
  conveyer
  (ensure-same "Common Lisp is good"
               (with-popen2 ("cat | grep good" conveyer pin pout)
                 (write-line "Haskell is bad" pin)
                 (write-line "Python is bad" pin)
                 (write-line "Common Lisp is good" pin)
                 (write-line "imho" pin)
                 (close pin)
                 (read-line pout))))
