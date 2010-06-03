;;;; t.lisp
;;;;
;;;; This file is part of the iolib.process library, released under MIT licence.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>


(defpackage #:iolib.process.test
  (:use #:cl #:lift #:iolib.process)
  (:export #:run-iolib.process-tests))

(in-package :iolib.process.test)

(defun run-iolib.process-tests ()
  (run-tests :suite 'iolib.process-test
             :report-pathname nil))

(deftestsuite iolib.process-test () ())

(addtest (iolib.process-test)
  simple1
  (ensure-same "Hello world"
               (with-child-process (python "python -c \"print 'Hello world'\"" :stdout t)
                 (read-line (process-output python)))))

(addtest (iolib.process-test)
  popen2-1
  (ensure-same "Hello Common Lisp"
               (with-child-process (python "python" :stdin t :stdout t)
                 (write-line "print 'Hello Common Lisp'"
                             (process-input python))
                 (process-input-close python)
                 (read-line (process-output python)))))

(addtest (iolib.process-test)
  popen2-2
  (ensure-same "Hello Common Lisp"
               (with-child-process (cat "cat" :stdin t :stdout t)
                 (write-line "Hello Common Lisp"
                             (process-input cat))
                 (process-input-close cat)
                 (read-line (process-output cat)))))

(addtest (iolib.process-test)
  popen3
  (ensure-same '("Common Lisp is good"
                 "Python is bad")
               (with-child-process (python "python" :stdin t :stdout t :stderr t)
                 (let ((*standard-output* (process-input python)))
                   (write-line "import sys")
                   (write-line "print 'Common Lisp is good'")
                   (write-line "print >> sys.stderr, 'Python is bad'"))
                 (process-input-close python)
                 (list (read-line (process-output python))
                       (read-line (process-error python))))))

(addtest (iolib.process-test)
  popen4
  (ensure-same '("Hello" "Hello")
               (with-child-process (python "python" :stdin t :union-stdout-stderr t)
                 (let ((*standard-output* (process-input python)))
                   (write-line "import sys")
                   (write-line "print 'Hello'")
                   (write-line "print >> sys.stderr, 'Hello'"))
                 (process-input-close python)
                 (let ((*standard-input* (process-output python)))
                   (list (read-line)
                         (read-line))))))

(addtest (iolib.process-test)
  conveyer
  (ensure-same "Common Lisp is good"
               (with-child-process (conveyer "cat | grep good" :stdin t :stdout t)
                 (let ((*standard-output* (process-input conveyer)))
                   (write-line "Haskell is bad")
                   (write-line "Python is bad")
                   (write-line "Common Lisp is good")
                   (write-line "imho"))
                 (process-input-close conveyer)
                 (read-line (process-output conveyer)))))


