
(defpackage "SYSTEM-MONITOR"
  (:nicknames "SYSMON")
  (:use "COMMON-LISP" "COMMON-LISP-USER")
  ;; (:shadow "READ-STAT-CPU")
  (:export "UPDATE-SYSTEM-INFO" "UPDATE-CPU-USAGE" "GET-CPU-USAGE")
  )

(in-package :sysmon)

(defvar *last-stat-cpu* nil)
(defvar *last-cpu-usage* '(:total 0 :user 0 :system 0 :from-last 0))

(defvar *SC_CLK_TCK* 2)
(sb-c-call:define-alien-routine "sysconf" sb-c-call:long (name sb-c-call:int))
(defvar *sc-clock-tick* 1) ;; dummy value
(setq *sc-clock-tick* (sysconf *SC_CLK_TCK*))

(defvar *sc-processor-number* 1)
(setq *sc-processor-number*
      (let ((num-processor-str
	     (with-output-to-string (s)
	       (sb-ext:run-program
		"/bin/sh"
		'("-c" "grep processor /proc/cpuinfo | wc -l")
		:output s))))
	(parse-integer num-processor-str)))

(defun update-system-info ()
  (update-cpu-usage))

(defun update-cpu-usage ()
  (let* ((cur-stat-cpu (read-stat-cpu))
	 (ret-cpu-usage
	  (if *last-stat-cpu*
	      (flet ((sum (stat-cpu)
		       (let ((ret 0))
			 (dolist (item stat-cpu)
			   (when (numberp item)
			     (setf ret (+ ret item))))
			 ret))
		     (diff-item (item-name)
		       (- (getf cur-stat-cpu item-name 0)
			  (getf *last-stat-cpu* item-name 0))))
		(let ((sum (- (sum cur-stat-cpu) (sum *last-stat-cpu*)))
		      (user (diff-item :user))
		      (system (diff-item :system)))
		  (list :total (/ (+ user system) sum)
			:user (/ user sum)
			:system (/ system sum)
			:from-last (/ sum
				      *sc-clock-tick*
				      *sc-processor-number*))))
	      *last-cpu-usage*)))
    (setq *last-stat-cpu* (read-stat-cpu))
    (setq *last-cpu-usage* ret-cpu-usage)
    ret-cpu-usage))

(defun get-cpu-usage ()
  *last-cpu-usage*)

(defun read-stat-cpu ()
  (with-open-file
      (s "/proc/stat")
    (let ((stat-cpu-line (read-line s)))
      (parse-proc-stat-cpu-line stat-cpu-line)
      )))

(defun parse-proc-stat-cpu-line (line-str)
  (with-input-from-string 
      (s (format nil "(~a)" line-str))
    (zigzag-merge
     '(:user :nice :system :idle :iowait :irq :softirq :steal :guest)
     (cdr (read s)))))

(defun zigzag-merge (lst1 lst2)
  (apply #'concatenate 'list (mapcar #'list lst1 lst2)))

(provide "SYSTEM-MONITOR")
