(defpackage :thread-group
  (:use :cl :lisp-unit)
  (:export
   #:thread-group
   #:start
   #:join
   #:stop
   #:add-lambda
   #:remove-lambda
   #:*runner*))

(in-package :thread-group)

(defvar *runner*)

(defclass thread-group ()
  ((tasks :initform (list)
          :accessor tasks)
   (threaded-tasks :initform (list)
                   :accessor threads)
   (checker :initform nil
            :accessor checker)
   (name :initarg :name
         :initform (princ-to-string (gensym "ThreadGroup"))
         :accessor name)))

(defun start (&key (runner *runner*) (check-sleep 5))
  (when (and (not (and (checker runner)
                       (bt:thread-alive-p (checker runner))))
             (not (zerop (length (tasks runner)))))
    (setf (checker runner)
          (bt:make-thread
           (lambda ()
             (unwind-protect
                  (loop
                     (dolist (task (tasks runner))
                       (let* ((name (car task))
                              (function (cdr task))
                              (thr (find name (threads runner) :key #'car :test #'equalp)))
                         (when (not (and (cdr thr) (bt:thread-alive-p (cdr thr))))
                           (stop-thread runner name)
                           (setf (threads runner) (remove name (threads runner) :key #'car :test #'equalp))
                           (push (cons name (bt:make-thread
                                             (lambda ()
                                               (let ((*runner* runner))
                                                 (funcall function))) :name (concatenate 'string (name runner) name)))
                                 (threads runner)))))
                     (sleep check-sleep))
               (when (checker runner)
                 (stop :runner runner))))
           :name (concatenate 'string (name runner) ":checker")))))

(defun join (&key (runner *runner*))
  (when (checker runner)
    (bt:join-thread (checker runner))))

(defun stop (&key (runner *runner*))
  (let ((threads (mapcar #'cdr (threads runner))))
    (when (checker runner)
      (push (checker runner) threads))
    (setf (checker runner) nil)
    (let ((thr (bt:current-thread)))
      (when (member thr threads :test #'eq)
        (setf threads (append (remove (bt:current-thread) threads :test #'eq) (list thr)))))
    (dolist (thr threads)
      (when (bt:thread-alive-p thr)
        (ignore-errors (sb-thread:terminate-thread thr))))))

(defun add-lambda (function &key (runner *runner*)
                         (name (princ-to-string
                                (gensym (princ-to-string (name runner))))))
  (push (cons name function) (tasks runner)))

(defun remove-lambda (name &key (runner *runner*))
  (stop-thread runner name)
  (setf (tasks runner) (remove name (tasks runner) :key #'car :test #'equalp))
  (setf (threads runner) (remove name (threads runner) :key #'car :test #'equalp)))

(defmethod stop-thread ((group thread-group) name)
  (let ((thr (find name (threads group) :key #'car :test #'equalp)))
    (when (and (cdr thr) (bt:thread-alive-p (cdr thr)))
      (sb-thread:terminate-thread (cdr thr)))))

(defmethod thread-start ((group thread-group)))

(remove-tests :all)

(defmacro with-setup (&body body)
  `(let ((threads-count (length (bt:all-threads)))
         (*runner* (make-instance 'thread-group)))
     (unwind-protect (progn ,@body)
       (stop))
     (sleep 0.001)
     (assert-eql threads-count (length (bt:all-threads)))))

(define-test return-when-empty
  (with-setup
    (start)
    (assert-false (join))))

(define-test dont-run-when-not-started
  (with-setup
    (let ((run nil))
      (add-lambda (lambda () (setf run t)))
      (sleep 0.001)
      (assert-false run))))

(define-test run-when-started
  (with-setup
    (let ((run nil))
      (add-lambda (lambda () (setf run t)))
      (start)
      (sleep 0.001)
      (assert-true run))))

(define-test remove-lambda-from-runner
  (with-setup
    (let ((run nil))
      (add-lambda (lambda () (setf run t)) :name "dont run")
      (remove-lambda "dont run")
      (start)
      (sleep 0.001)
      (assert-false run))))

(define-test remove-lambda-from-running-runner
  (with-setup
    (let ((run nil)
          (long-run nil))
      (add-lambda (lambda () (setf run t) (sleep 1) (setf long-run t)) :name "short run")
      (start)
      (sleep 0.001)
      (remove-lambda "short run")
      (sleep 0.001)
      (assert-true run)
      (assert-false long-run))))

(define-test ensure-restarting-failed-tasks
  (with-setup
    (let ((run nil)
          (restart nil))
      (add-lambda (lambda () (if (not run)
                       (setf run t)
                       (setf restart t))))
      (start :check-sleep 0.0001)
      (sleep 0.01)
      (assert-true run)
      (assert-true restart))))

(define-test overriding-existing-lambda
  (with-setup
    (let ((arun nil)
          (brun nil))
      (add-lambda (lambda () (setf arun t)) :name "overrun")
      (start :check-sleep 0.000001)
      (sleep 0.00001)
      (add-lambda (lambda () (setf brun t)) :name "overrun")
      (sleep 0.01)
      (assert-true arun)
      (assert-true brun))))

(define-test adding-multiple-lambdas
  (with-setup
    (let ((arun nil)
          (brun nil))
      (add-lambda (lambda () (setf arun t)))
      (add-lambda (lambda () (setf brun t)))
      (start)
      (sleep 0.001)
      (assert-true arun)
      (assert-true brun))))

(define-test stop-from-lambda
  (with-setup
    (let ((arun nil)
          (brun nil)
          (crun nil))
      (add-lambda (lambda () (sleep 0.002) (setf arun t)))
      (add-lambda (lambda () (setf crun t)))
      (add-lambda (lambda () (sleep 0.001) (stop)))
      (add-lambda (lambda () (sleep 0.01) (setf brun t)))
      (start)
      (sleep 0.1)
      (stop)
      (assert-false arun)
      (assert-false brun)
      (assert-true crun))))

(define-test stop-immediately-after-start
  (with-setup
    (let ((arun nil)
          (brun nil))
      (add-lambda (lambda () (sleep 0.09) (setf arun t)))
      (add-lambda (lambda () (stop)))
      (add-lambda (lambda () (sleep 0.09) (setf brun t)))
      (start)
      (sleep 0.1)
      (assert-false arun)
      (assert-false brun))))

(define-test dont-execute-after-stop
  (with-setup
    (let ((run nil))
      (add-lambda (lambda () (stop) (setf run t)))
      (assert-false run))))

(define-test stop-group-when-checker-is-interrupted
  (with-setup
    (let ((thread-count (length (bt:all-threads))))
      (add-lambda (lambda () (loop (sleep 1))))
      (start)
      (sleep 0.001)
      (sb-thread:terminate-thread (checker *runner*))
      (sleep 0.001)
      (assert-eql thread-count (length (bt:all-threads))))))

(define-test multiple-starts-are-ignorred
  (with-setup
    (let ((thread-count (length (bt:all-threads))))
      (add-lambda (lambda () (loop (sleep 1))))
      (start)
      (start)
      (sleep 0.001)
      (start)
      (sleep 0.001)
      (assert-eql (+ 2 thread-count) (length (bt:all-threads))))))

(define-test stopping-own-lambda
  (with-setup
    (let ((run nil)
          (cleanup nil))
      (add-lambda (lambda () (unwind-protect (progn (stop) (setf run t))
                     (setf cleanup t))))
      (start)
      (sleep 0.01)
      (assert-false run)
      (assert-true cleanup))))

(define-test stop-when-one-fails-when-quitter)

(let ((*print-failures* t)
      (*print-errors* t))
  (run-tests :all))
