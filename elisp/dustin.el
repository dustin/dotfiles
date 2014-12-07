; Miscellaneous libraryish stuff for myself.

(defun dustin-join (somelist by)
  "Join a list of strings by the given join string."
  (mapconcat (lambda (x) x) somelist by))

(defun dustin-maybe-plural (n word)
  "Pluralize a word (by placing an s on the end) if n is not 1"
  (if (= n 1)
      word
    (concat word "s")))

(defun dustin-filter (condp lst)
  (delq nil
        (mapcar (lambda (x) (and (funcall condp x) x)) lst)))

(defun humanize-size (n)
  "Humanize a size in bytes."
  (let ((prefixes ["B" "KiB" "MiB" "GiB" "TiB" "PiB" "EiB"])
        (e (floor (/ (log n) (log 1024)))))
    (format "%.2f%s" (/ (float n) (expt 1024 (floor e))) (elt prefixes e))))

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

(provide 'dustin)
