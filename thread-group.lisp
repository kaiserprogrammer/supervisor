(defpackage :thread-group
  (:use :cl)
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
         :accessor name)
   (lock :accessor lock
         :initform (bt:make-lock))))

(defun start (&key (runner *runner*) (check-sleep 5))
  (when (and (not (and (checker runner)
                       (bt:thread-alive-p (checker runner))))
             (not (zerop (length (tasks runner)))))
    (setf (checker runner)
          (bt:make-thread
           (lambda ()
             (unwind-protect
                  (loop
                     (bt:with-lock-held ((lock runner))
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
                                                   (funcall function))) :name (concatenate 'string (name runner) ":" name)))
                                   (threads runner))))))
                     (sleep check-sleep))
               (when (checker runner)
                 (setf (checker runner) nil)
                 (stop :runner runner))))
           :name (concatenate 'string (name runner) ":checker")))))

(defun join (&key (runner *runner*))
  (when (checker runner)
    (bt:join-thread (checker runner))))

(defun stop (&key (runner *runner*))
  (when (checker runner)
    (bt:with-lock-held ((lock runner))
      (let ((thr (checker runner)))
        (setf (checker runner) nil)
        (when (bt:thread-alive-p thr)
          (ignore-errors (bt:destroy-thread thr))))))
  (let ((threads (mapcar #'cdr (threads runner))))
    (let ((thr (find (bt:current-thread) threads :test #'eq)))
      (when thr
        (setf threads (remove thr threads :test #'eq)))
      (dolist (thr threads)
        (when (bt:thread-alive-p thr)
          (ignore-errors (bt:destroy-thread thr))))
      (when thr
        (handler-bind
            ((BORDEAUX-THREADS::BORDEAUX-MP-CONDITION
              (lambda (c)
                (declare (ignore c))
                (let ((restart (find-restart 'abort)))
                  (when restart
                    (invoke-restart restart))))))
          (bt:destroy-thread thr))))))

(defun add-lambda (function &key (runner *runner*)
                         (name (princ-to-string
                                (gensym))))
  (push (cons name function) (tasks runner)))

(defun remove-lambda (name &key (runner *runner*))
  (stop-thread runner name)
  (setf (tasks runner) (remove name (tasks runner) :key #'car :test #'equalp))
  (setf (threads runner) (remove name (threads runner) :key #'car :test #'equalp)))

(defmethod stop-thread ((group thread-group) name)
  (let ((thr (find name (threads group) :key #'car :test #'equalp)))
    (when (and (cdr thr) (bt:thread-alive-p (cdr thr)))
      (bt:destroy-thread (cdr thr)))))
