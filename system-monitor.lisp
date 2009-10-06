
(asdf:operate 'asdf:load-op 'cl-ppcre)

(defpackage "SYSTEM-MONITOR"
  (:nicknames "SYSMON")
  (:use "COMMON-LISP" "COMMON-LISP-USER")
  ;; (:shadow "READ-STAT-CPU")
  (:export "UPDATE-SYSTEM-INFO" "UPDATE-CPU-USAGE" "UPDATE-NET-USAGE"
	   "GET-CPU-USAGE" "GET-MEMORY-USAGE" "GET-NET-USAGE"
	   "GET-BATTERY-GRAPH"
	   )
  )

(in-package :sysmon)

(defvar *last-stat-cpu* nil)
(defvar *last-cpu-usage* '(:total 0 :user 0 :system 0 :iowait 0 :from-last 0))

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
  (update-cpu-usage)
  (update-net-usage))

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
		      (system (diff-item :system))
		      (iowait (diff-item :iowait)))
		  (if (= sum 0)
		      '(:total 0 :user 0 :system 0 :iowait 0 :from-last 0)
		      (list :total (/ (+ user system) sum)
			    :user (/ user sum)
			    :system (/ system sum)
			    :iowait (/ iowait sum)
			    :from-last (/ sum
					  *sc-clock-tick*
					  *sc-processor-number*)))))
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

(defun get-memory-usage ()
  (let ((memtotal -1)
	(memused -1)
	(memfree -1)
	(memcached -1))
    (flet ((get-num (line)
	     (parse-integer line
			    :junk-allowed t
			    :start (+ 1 (search ":" line)))))
      (with-open-file (s "/proc/meminfo")
	(loop while (or (= memtotal -1)
			(= memfree -1)
			(= memcached -1))
	     do
	     (let ((line (read-line s)))
	       (cond
		 ((search "MemTotal:" line)
		  (setf memtotal (get-num line)))
		 ((search "MemFree:" line)
		  (setf memfree (get-num line)))
		 ((search "Cached:" line)
		  (setf memcached (get-num line)))))))
      (setf memused (- memtotal memfree))
      (if (= memtotal 0)
	  '(:used 0 :cached 0)
	  (list :used (/ (- memused memcached) memtotal)
		:cached (/ memcached memtotal))))))

(defvar *last-net-usage* 
  '(:recv 0 :send 0 :from-last 0))
(defvar *last-net-dev* nil)
(defvar *last-net-dev-time* 0)

(defun update-net-usage ()
  (flet ((parse-dev-line (line)
	   (let* ((colon-idx (search ":" line))
		  (iface (string-trim " " (subseq line 0 colon-idx)))
		  (numline (format nil "(~a)" (subseq line (1+ colon-idx))))
		  (nums (with-input-from-string (s numline) (read s)))
		  (nums (zigzag-merge '(:recv-bytes :recv-packets :recv-errs 
					:recv-drop :recv-fifo :recv-frame
					:recv-compressed :recv-multicast
					:trans-bytes :trans-packets
					:trans-errs :trans-drop
					:trans-fifo :trans-colls
					:trans-carrier :trans-compressed)
				      nums)))
	     (list iface nums)))
	 (summarize-dev (net-dev)
	   (list :recv
		 (apply #'+ (mapcar (lambda (x) (getf (cadr x) :recv-bytes))
				    net-dev))
		 :send
		 (apply #'+ (mapcar (lambda (x) (getf (cadr x) :trans-bytes))
				    net-dev)))))
    (let* ((net-dev
	    (with-open-file (s "/proc/net/dev")
	      (read-line s)(read-line s) ;; skip headers
	      (let ((ret nil))
		(handler-case
		    (loop while t
		       do
		       (let ((dev-line (parse-dev-line (read-line s))))
			 ;; filter local loopback
			 (unless (equal "lo" (car dev-line))
			   (push dev-line ret))))
		  (end-of-file (err) ret)))))
	   (last-dev (summarize-dev *last-net-dev*))
	   (cur-dev (summarize-dev net-dev))
	   (usage (list :recv (- (getf cur-dev :recv)
				 (getf last-dev :recv 0))
			:send (- (getf cur-dev :send)
				 (getf last-dev :send 0))
			:from-last
			(let ((now (get-internal-real-time)))
			  (if *last-net-dev-time*
			      (/ (- (get-internal-real-time)
				    *last-net-dev-time*)
				 internal-time-units-per-second)
			      1)))))
      (setf *last-net-dev-time* (get-internal-real-time))
      (setf *last-net-dev* net-dev)
      (setf *last-net-usage* usage)
      usage)))

(defun get-net-usage ()
  *last-net-usage*)

(defun get-battery-graph ()
  (let* ((acpi-str
	  (with-output-to-string (s)
	    (sb-ext:run-program "/bin/sh" '("-c" "acpi") :output s)))
	 (charged-ratio
	  (parse-integer acpi-str :start (cl-ppcre:scan "\\d+%" acpi-str)
			 :junk-allowed t))
	 (state (cond ((search "Discharging" acpi-str) 'discharging)
		      ((search "100%" acpi-str) 'full-charged)
		      (t 'charging))))
    (format nil "~A~A~a[~A]"
	    (make-string (floor (/ charged-ratio 10))
			 :initial-element #\@)
	    (let ((digit (mod charged-ratio 10)))
	      (if (= 0 digit)
		  ""
		  (cond ((> digit 7) "O")
			((> digit 4) "o")
			(t "."))))
	    (make-string (- 10 (ceiling (/ charged-ratio 10)))
			 :initial-element #\_)
	    (if (eq state 'discharging)
		"=//=" "===="))
  ))

(provide "SYSTEM-MONITOR")
