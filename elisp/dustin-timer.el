(require 'dustin)

;; I like midnight mode, but I don't want it to run at midnight.
(defvar dustin-periodic-timer nil
  "Timer for supporting some periodic cleanup tasks.

Currently runs dustin-periodic-task about every four hours.")

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
               (mapconcat (lambda (x) x) (reverse out-words) ", ")))))
    (funcall f duration '() (reverse timer-duration-words))))

(defun dustin-timer-next-run-in-words (timer)
  "How long until the given timer runs (in English)?"
  (duration-in-words (dustin-timer-next-run timer)))

(defun dustin-periodic-task ()
  "A task to run every once in a while.

This runs dustin-periodic-task-hooks after doing its normal thing."

  ; run-at-time stacks a bunch of these things up, so ensure that the next
  ; execution is in the future before proceeding here.
  (if (> (dustin-timer-next-run dustin-periodic-timer) 0)
      (progn
        (message "Doing periodic cleanup stuff soon.")
        (run-with-idle-timer 10 nil (lambda ()
                                      (clean-buffer-list)
                                      (run-hooks 'dustin-periodic-task-hooks))))))

(defun dustin-filter (condp lst)
  (delq nil
        (mapcar (lambda (x) (and (funcall condp x) x)) lst)))

(defun dustin-schedule-periodic (period)
  "Schedule the timer to run on the given period."
  (if dustin-periodic-timer
      (cancel-timer dustin-periodic-timer))
  (setq dustin-periodic-timer
        (run-at-time period period 'dustin-periodic-task)))

(defun dustin-cleanup-dir (dirname oldest)
  "Remove any old files from a directory.  oldest is the maximum file age (in seconds) to keep."
  (let ((filenames
         (mapcar (lambda (x) (car x))
                 (dustin-filter (lambda (x)
                                  (and (not (or (string-equal (car x) ".")
                                                (string-equal (car x) "..")))
                                       (not (nth 0 (cdr x))) ;; is a directory
                                       (> (float-time (time-subtract (current-time) (nth 5 (cdr x))))
                                          oldest)))
                                (directory-files-and-attributes dirname)))))
    (dolist (fn (mapcar (lambda (x) (mapconcat 'identity (list dirname x) "/")) filenames))
      (message "Deleting %s" fn)
      (delete-file fn t))))

(provide 'dustin-timer)
