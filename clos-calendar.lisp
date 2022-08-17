;;; I am writing this because I'm sick of GNOME Calendar crashing all the time.

(in-package #:clos-calendar)

;;; Saving to files

(defvar *calendar-save-path* #p".")

(defun get-saveable-slot-values (obj)
  (let* ((class (class-of obj))
         (slots (remove-if-not 'c2mop:slot-definition-initargs (c2mop:class-slots class))))
    (loop :for slot :in slots
          :collect (list (first (c2mop:slot-definition-initargs slot))
                         (simplify-for-saving (slot-value obj (c2mop:slot-definition-name slot)))))))

(defun make-object-saveable (obj)
  (append (list (class-name (class-of obj)))
          (get-saveable-slot-values obj)))

(defgeneric simplify-for-saving (obj)
  (:documentation "Returns an object that can be serialized."))

(defmethod simplify-for-saving ((obj string))
  obj)

(defmethod simplify-for-saving ((obj number))
  obj)

(defmethod simplify-for-saving ((obj symbol))
  obj)

(defmethod simplify-for-saving ((obj list))
  (mapcar 'simplify-for-saving obj))

(defmethod simplify-for-saving ((obj time:timestamp))
  (list :timestamp (time:format-timestring nil obj)))

(defun save-as-sexps (save-obj stream)
  (dolist (event save-obj)
    (write-char #\( stream)
    (dolist (obj event)
      (write obj :stream stream :pretty t)
      (terpri stream))
    (write-char #\) stream)))

(defun save-calendar-to-file (calendar)
  (let ((*package* (find-package '#:cl-calendar)))
    (with-open-file (stream (merge-pathnames
                             (format nil "~A.calendar" (name calendar))
                             *calendar-save-path*)
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (format stream ";;;; -*- mode: lisp-data -*-~%")
      (format stream "\"~A\"~%" (name calendar))
      (save-as-sexps (simplify-for-saving
                      (coerce (sorted-events calendar) 'list))
                     stream)
      T)))

(defun parse-saved-obj (obj-list)
  (cond ((and (listp obj-list) (eq :timestamp (first obj-list)))
         (time:parse-timestring (second obj-list)))
        ((and (listp obj-list) (keywordp (first obj-list))
              (= (length obj-list) 2)
              (list (first obj-list) (parse-saved-obj (second obj-list)))))
        ((and (listp obj-list) (ignore-errors (find-class (first obj-list))))
         (apply 'make-instance
                (first obj-list)
                (apply 'append (mapcar 'parse-saved-obj (rest obj-list)))))
        ((listp obj-list)
         (mapcar 'parse-saved-obj obj-list))
        (t obj-list)))

(defun load-calendar-from-file (calendar-name)
  (flet ((fix-timestamps (initarg-list)
           (loop :for initarg :in initarg-list
                 :collect (cond
                            ((and (listp (second initarg))
                                  (eq (first (second initarg)) :timestamp))
                             (list (first initarg) (time:parse-timestring (cadadr initarg))))
                            (t initarg)))))
    (with-open-file (stream (merge-pathnames
                             (format nil "~A.calendar" calendar-name)
                             *calendar-save-path*))
      (when (string-not-equal (read stream) calendar-name)
        (error "Calendar name and file name don't match."))
      (let ((events '())
            (*package* (find-package '#:cl-calendar)))
        (handler-case
            (loop
              (let ((obj (parse-saved-obj (read stream))))
                (cond ((typep obj 'entry)
                       (push obj events))
                      ((and (typep obj 'reminder) (not (has-fired-p obj)))
                       (register-reminder obj))
                      (t (error "Invalid object in calendar.")))))
          (end-of-file () (make-instance 'calendar :events events :name calendar-name)))))))

(defvar *home-timezone* time:*default-timezone*)

(defun make-timestamp (year month day &key
                                        (hour 0) (minute 0) (second 0)
                                        (nsec 0))
  (time:encode-timestamp nsec second minute hour day month year))

(defun get-appropriate-time ()
  "Automagically returns an appropriate time based on what's going on in the UI.
TODO: Make something that lets a UI actually affect this."
  (multiple-value-bind (second minute hour day month year)
      (get-decoded-time)
    (declare (ignore second))
    (make-timestamp year month day :hour hour :minute minute)))

(defun timestamp-pretty-string (timestamp &key time-only-p)
  (let* ((time-only '((:hour 2) #\: (:min 2)))
         (full (append
                '((:year 4) #\- (:month 2) #\- (:day 2) #\Space)
                time-only)))
    (time:format-timestring nil timestamp :format (if time-only-p time-only full))))

(defclass calendar-view-item-mixin () ())
(defclass task-view-item-mixin () ())

(defclass entry ()
  ((%name :accessor name :initarg :name :initform "Unnamed Event")
   (%notes :accessor notes :initarg :notes :initform nil)
   (%location :accessor location :initarg :location :initform nil)
   (%category :accessor category :initarg :category :initform nil)
   (%start-time :accessor start-time :type timestamp :initarg :start-time :initform (get-appropriate-time))
   (%end-time :accessor end-time :type (or timestamp null) :initarg :end-time :initform nil)
   (%repeat :accessor repeat :initarg :repeat :initform nil)
   (%reminders :accessor reminders :initarg :reminders :initform '())
   (%hypothetical-unsaveable-slot :accessor hypothetical-unsaveable-slot)
   (%unusable-initargs :accessor unusable-initargs :initarg :unusable :initform '())))

(defmethod print-object ((object entry) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "\"~A\"" (name object))))

(defmethod simplify-for-saving ((obj entry))
  (make-object-saveable obj))

(defmethod duration-as-string ((entry entry))
  (with-output-to-string (stream)
    (format stream "~A" (start-time entry))
    (when (end-time entry)
      (format stream " ~~ ~A" (end-time entry)))))

(defclass event (entry calendar-view-item-mixin)
  ((%lasts-all-day-p :accessor lasts-all-day-p :initarg :lasts-all-day-p :initform nil)))

(defmethod print-object ((object event) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "\"~A\" ~A"
            (name object) (timestamp-pretty-string (start-time object)))
    (when (end-time object)
      (format stream " ~~ ~A" (timestamp-pretty-string (end-time object))))))

(defmethod in-the-past-p ((event event))
  (time:timestamp< (if (end-time event) (end-time event) (start-time event)) (time:now)))

(defmethod in-the-future-p ((event event))
  (time:timestamp< (time:now) (start-time event)))

(defmethod happening-now-p ((event event))
  (when (end-time event)
    (and (time:timestamp>= (end-time event) (time:now))
         (time:timestamp<= (start-time event) (time:now)))))

(defclass task (entry task-view-item-mixin)
  ((%completed-p :accessor completed-p :initform nil)))

(defmethod in-the-past-p ((task task))
  (time:timestamp< (deadline task) (time:now)))

(defmethod overdue-p ((task task))
  (and (not (completed-p task)) (in-the-past-p task)))

(defclass calendar ()
  ((%name :accessor name :initarg :name :initform "Unnamed Calendar")
   (%events :accessor calendar-events
            :initform '()
            :initarg :events)))

(defmethod add-event ((calendar calendar) (event entry))
  (push event (calendar-events calendar))
  event)

(defmethod remove-event ((calendar calendar) (event entry))
  (setf (calendar-events calendar) (remove event (calendar-events calendar))))

(defmethod list-of-events ((calendar calendar))
  (copy-list (calendar-events calendar)))

(defmethod sorted-events ((calendar calendar))
  (sort (list-of-events calendar)
        (lambda (a b)
          (time:timestamp< (start-time a)
                           (start-time b)))))

(defun print-all-events (calendar)
  (loop :for event :in (sorted-events calendar
                                      (lambda (event-a event-b)
                                        (time:timestamp< (start-time event-a)
                                                         (start-time event-b))))
        :do (format t "\"~A\" ~A ~C ~A~%"
                    (name event) (start-time event) #\~ (end-time event))))

(defmethod future-events ((calendar calendar))
  (remove-if-not (lambda (x) (or (in-the-future-p x) (happening-now-p x)))
                 (list-of-events calendar)))

;;; Reminders (none of this is to be considered finished, I made it in a hurry)
(defclass reminder ()
  ((%event-name :accessor event-name :initarg :event-name)
   (%event-start-time :accessor event-start-time :initarg :event-start-time)))

(defgeneric should-fire-p (reminder))
(defgeneric has-fired-p (reminder))
(defgeneric fire-reminder (reminder))

(defclass standard-reminder (reminder)
  ((%reminder-text :accessor reminder-text :initarg :reminder-text)
   (%reminder-time :accessor reminder-time :initarg :reminder-time)
   (%has-fired-p :accessor has-fired-p :initform nil :initarg :has-fired-p)))

(defmethod simplify-for-saving ((obj standard-reminder))
  (make-object-saveable obj))

(defmethod fire-reminder ((reminder standard-reminder))
  (unless (has-fired-p reminder)
    (setf (has-fired-p reminder) t)
    (msgbox:show (format nil "Calendar reminder!~%~A" (reminder-text reminder)))))

(defmethod should-fire-p ((reminder standard-reminder))
  (and (not (has-fired-p reminder))
       (time:timestamp< (reminder-time reminder) (time:now))))

(defun make-reminder-for-event (event &optional reminder-time)
  (let ((reminder-time (or reminder-time (start-time event))))
    (make-instance 'standard-reminder
                   :reminder-text (format nil "~A~%~A~%Duration: ~A"
                                          (name event)
                                          (location event)
                                          (duration-as-string event))
                   :reminder-time reminder-time
                   :event-name (name event)
                   :event-start-time (start-time event))))

(defmethod add-reminder ((reminder reminder) (entry entry))
  (push reminder (reminders entry))
  reminder)

;;; Reminder monitor (also made in a hurry because I needed reminders)
(defvar *reminders* '())
(defparameter *quit-reminder-monitor-p* nil)
(defparameter *reminder-monitor-delay* 5
  "How many seconds to wait between each reminder check.")

(defun reminder-monitor (&key calendar-name refresh-each-check-p)
  (let (calendar)
    (flet ((load-cal ()
             (setf calendar (load-calendar-from-file calendar-name))))
      (when calendar-name
        (load-cal)
        (register-all-reminders calendar))
      (do ((reminders (copy-list *reminders*)
                      (if refresh-each-check-p
                          (progn
                            (load-cal)
                            (register-all-reminders calendar)
                            *reminders*)
                          (copy-list *reminders*))))
          (*quit-reminder-monitor-p*)
        (dolist (reminder *reminders*)
          (when (should-fire-p reminder)
            (fire-reminder reminder)
            (save-calendar-to-file calendar)))
        (sleep *reminder-monitor-delay*))))
  (format t "Quit the reminder monitor.~%")
  (force-output))

(defun register-reminder (reminder)
  (push reminder *reminders*))

(defun register-all-reminders (calendar)
  (dolist (reminders (mapcar 'reminders (list-of-events calendar)))
    (dolist (reminder (remove-if #'has-fired-p reminders))
      (register-reminder reminder)))
  (setf *reminders* (remove-duplicates *reminders*)))
