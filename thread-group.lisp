(defpackage :thread-group
  (:use :cl :lisp-unit))

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
         :initform (concatenate 'string (princ-to-string (gensym "ThreadGroup")) ":")
         :accessor name)))

(defun start (&key (runner *runner*) (check-sleep 5))
  (when (and (not (and (checker runner)
                       (bt:thread-alive-p (checker runner))))
             (not (zerop (length (tasks runner)))))
    (setf (checker runner)
          (bt:make-thread
           (lambda ()
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
                (sleep check-sleep)))
           :name (concatenate 'string (name runner) " checker")))))

(defun join (&key (runner *runner*))
  (when (checker runner)
    (bt:join-thread (checker runner))))

(defun stop (&key (runner *runner*))
  (let ((threads (mapcar #'cdr (threads runner))))
    (when (checker runner)
      (push (checker runner) threads))
    (dolist (thr threads)
      (when (bt:thread-alive-p thr)
        (ignore-errors (bt:destroy-thread thr)))))
  (setf (checker runner) nil))

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
      (bt:destroy-thread (cdr thr)))))

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
    (let ((arun nil))
      (add-lambda (lambda () (sleep 0.002) (setf arun t)))
      (add-lambda (lambda () (sleep 0.001) (stop)))
      (start)
      (sleep 0.01)
      (assert-false arun))))

(define-test stop-immediately-after-start
  (with-setup
    (let ((arun nil))
      (add-lambda (lambda () (sleep 0.002) (setf arun t)))
      (add-lambda (lambda () (stop)))
      (start)
      (sleep 0.01)
      (assert-false arun))))

(let ((*print-failures* t)
      (*print-errors* t))
  (run-tests :all))
