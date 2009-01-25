(require 'dustin)

;; I like midnight mode, but I don't want it to run at midnight.
(defvar dustin-periodic-timer nil
  "Timer for supporting some periodic cleanup tasks.

Currently runs dustin-periodic-task about every four hours.")

(defun dustin-periodic-task ()
  "A task to run every once in a while.

This runs dustin-periodic-task-hooks after doing its normal thing."
  (message "Doing periodic cleanup stuff soon.")
  (run-with-idle-timer 10 nil (lambda ()
                               (clean-buffer-list)
                               (run-hooks 'dustin-periodic-task-hooks))))

(setq dustin-periodic-timer
      (run-at-time "4 hours" (* 4 60 60) 'dustin-periodic-task))

(defun dustin-timer-next-run (timer)
  "How long until the given timer runs?"
  (-
   (time-to-seconds (timer--time timer))
   (time-to-seconds (current-time))))

(defun duration-in-words (duration)
  "How long is this number of seconds in meaningful units?"
  (let ((f (lambda (current out-words in-words)
             (if in-words
                 (let* ((multiple (floor (/ current (cdar in-words))))
                       (remaining (- current (* multiple (cdar in-words)))))
                   (if (>= multiple 1)
                       (funcall f remaining
                                (cons
                                 (format "%d %s" multiple
                                         (dustin-maybe-plural multiple (caar in-words)))
                                 out-words)
                                (cdr in-words))
                     (funcall f current out-words (cdr in-words))))
               (dustin-join (reverse out-words) ", ")))))
    (funcall f duration '() (reverse timer-duration-words))))

(defun dustin-timer-next-run-in-words (timer)
  "How long until the given timer runs (in English)?"
  (duration-in-words (dustin-timer-next-run timer)))

(provide 'dustin-timer)