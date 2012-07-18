; Miscellaneous libraryish stuff for myself.

(defun dustin-join (somelist by)
  "Join a list of strings by the given join string."
  (mapconcat (lambda (x) x) somelist by))

(defun dustin-maybe-plural (n word)
  "Pluralize a word (by placing an s on the end) if n is not 1"
  (if (= n 1)
      word
    (concat word "s")))

(defun humanize-size (n)
  "Humanize a size in bytes."
  (let ((prefixes ["B" "KiB" "MiB" "GiB" "TiB" "PiB" "EiB"])
        (e (floor (/ (log n) (log 1024)))))
    (format "%.2f%s" (/ (float n) (expt 1024 (floor e))) (elt prefixes e))))

(provide 'dustin)
