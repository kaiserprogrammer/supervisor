(defpackage :supervisor.test
  (:use :cl :supervisor :lisp-unit))
(in-package :supervisor.test)

(remove-tests :all)

(defmacro with-setup (&body body)
  `(let ((threads-count (length (bt:all-threads)))
         (*runner* (make-instance 'supervisor)))
     (unwind-protect (progn ,@body)
       (stop))
     (sleep 0.001)
     (assert-eql threads-count (length (bt:all-threads)))))

(define-test return-when-empty
  (with-setup
    (start)
    (assert-false (join))))

(define-test fast-stop
  (let ((*runner* (make-instance 'supervisor))
        (thread-count (length (bt:all-threads)))
        (run nil))
    (add-lambda (lambda () (setf run t) (stop)))
    (start)
    (sleep 0.001)
    (assert-true run)
    (assert-eql thread-count (length (bt:all-threads)))))

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
          (brun nil)
          (crun nil))
      (add-lambda (lambda () (sleep 0.01) (setf arun t) (loop (sleep 1))) :name "Arunner")
      (add-lambda (lambda () (progn
                     (setf crun t)
                     (stop)
                     (sleep 0.01)
                     (setf crun nil)))
             :name "Crunner")
      (add-lambda (lambda () (sleep 0.01) (setf brun t)) :name "Brunner")
      (start)
      (sleep 0.1)
      (assert-true crun)
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
      (bt:destroy-thread (supervisor::checker *runner*))
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
      (add-lambda (lambda () (sleep 0.0001)
                (unwind-protect (progn (stop) (setf run t))
                  (setf cleanup t))))
      (start)
      (sleep 0.01)
      (assert-false run)
      (assert-true cleanup))))

(define-test stop-when-one-fails-when-quitter)

(let ((*print-failures* t)
      (*print-errors* t))
  (supervisor.test::run-tests :all))
