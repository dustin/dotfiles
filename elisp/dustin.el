; Miscellaneous libraryish stuff for myself.

(defun dustin-join (somelist by)
  "Join a list of strings by the given join string."
  (substring
   (apply 'concat (mapcar (lambda (x) (concat x by)) somelist))
   0 -2))

(defun dustin-maybe-plural (n word)
  "Pluralize a word (by placing an s on the end) if n is not 1"
  (if (= n 1)
      word
    (concat word "s")))

(provide 'dustin)