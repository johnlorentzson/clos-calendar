(in-package #:clos-calendar.crappy-tui)

(defvar *active-calendar*)

(defparameter *command-table*
  '(#\a display-all-events
    #\d describe-event
    #\f display-future-events
    #\c select-calendar
    #\q quit-calendar
    #\n new-event
    #\w write-calendar))

(defun command (cmd-string)
  (handler-case
      (let ((command-char (elt cmd-string 0)))
        (funcall (getf *command-table* command-char) (subseq cmd-string 1)))
    (no-such-event (c) (princ c))))

(define-condition quit ()
  ())

(define-condition no-such-event ()
  ((%event-number :accessor event-number :initarg :number))
  (:report
   (lambda (condition stream)
     (format stream "No such event ~D.~%" (event-number condition)))))

(defun calendar-top-level ()
  (handler-case
      (loop
        (write-char #\:)
        (force-output)
        (with-simple-restart (restart "Return to calendar top level.")
          (command (read-line))))
    (quit () (format t "Exiting calendar top level."))))

(defun select-calendar (args)
  (handler-case
      (progn
        (setf *active-calendar* (cal:load-calendar-from-file args))
        (format t "Loaded calendar.~%"))
    (error (c) (format t "Failed to load calendar. Error:~%~A~%" c))))

(defun write-calendar (args)
  (declare (ignore args))
  (cal:save-calendar-to-file *active-calendar*))

(defun print-event (index event)
  (format t "~D. \"~A\" ~A ~~ ~A~%"
          index (cal:name event)
          (cal:timestamp-pretty-string (cal:start-time event))
          (if (cal:end-time event)
              (cal:timestamp-pretty-string (cal:end-time event))
              nil)))

(defun display-all-events (args)
  (let ((start (if (string= args "")
                   nil
                   (quick-and-dirty-timestamp args))))
    (loop :for event :in (cal:sorted-events *active-calendar*)
          :for index :from 1
          :when (or (null start) (time:timestamp>= (cal:start-time event) start))
            :do (print-event index event))))

(defun display-future-events (args)
  (declare (ignore args))
  (let ((future (cal:future-events *active-calendar*)))
    (loop :for event :in (cal:sorted-events *active-calendar*)
          :for index :from 1
          :when (member event future) :do
            (print-event index event))))

(defun describe-event (args)
  (when (or (> (parse-integer args) (length (cal:list-of-events *active-calendar*)))
            (minusp (parse-integer args)))
    (error 'no-such-event :number (parse-integer args)))
  (let ((event (nth (1- (parse-integer args)) (cal:sorted-events *active-calendar*))))
    (format t "~A~%Location: ~A~%Starts ~A~%" (cal:name event)
            (cal:location event)
            (cal:timestamp-pretty-string (cal:start-time event)))
    (when (cal:end-time event)
      (format t "Ends ~A~%" (cal:timestamp-pretty-string (cal:end-time event))))
    (when (cal:lasts-all-day-p event)
      (format t "Lasts all day.~%"))
    (format t "~%~A~%" (cal:notes event))))

(defun make-timestamp-tui (year month day &optional (hour 0) (minute 0) (sec 0))
  (cal:make-timestamp year month day :hour hour :minute minute :second sec))

(defun quick-and-dirty-timestamp (string)
  (if (string= "" string)
      nil
      (apply 'make-timestamp-tui (mapcar #'parse-integer (str:split #\Space string)))))

(defun new-event (args)
  (declare (ignore args))
  (unless (boundp '*active-calendar*)
    (error "Can't create new event without active calendar."))
  (flet ((prompt (message &optional (term-char #\Newline))
           (format t "~A: " message)
           (force-output)
           ;; TODO: use term-char
           (read-line)))
    (let ((event (make-instance 'cal:event
                                :name (prompt "Name")
                                :notes (prompt "Notes (end with \\)")
                                :start-time (quick-and-dirty-timestamp (prompt "Start time"))
                                :end-time (quick-and-dirty-timestamp (prompt "End time"))
                                :lasts-all-day-p (prompt "Lasts all day? (T or NIL)")
                                :location (prompt "Location"))))
      (cal:add-event *active-calendar* event))))

(defun quit-calendar (args)
  (declare (ignore args))
  (signal 'quit))
