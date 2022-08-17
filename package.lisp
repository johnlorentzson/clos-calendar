;;;; package.lisp

(defpackage #:clos-calendar
  (:use #:cl)
  (:local-nicknames (#:time #:local-time)
                    (#:msgbox #:org.shirakumo.messagebox))
  (:export #:save-calendar-to-file #:load-calendar-from-file
           #:make-timestamp #:timestamp-pretty-string
           #:calendar-view-item-mixin #:task-view-item-mixin
           #:entry #:event #:task
           #:in-the-past-p #:in-the-future-p #:happening-now-p #:overdue-p
           #:calendar #:add-event #:list-of-events #:sorted-events #:future-events

           ;; Slot accessors
           #:name #:notes #:location #:category #:start-time #:end-time #:repeat
           #:reminders #:lasts-all-day-p #:completed-p))

(defpackage #:cl-calendar.crappy-tui
  (:use #:cl)
  (:local-nicknames (#:cal #:clos-calendar)
                    (#:time #:local-time))
  (:export #:calendar-top-level))
