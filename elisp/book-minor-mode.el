(defun book-wrap (b e &optional each)
  (save-excursion
    (save-restriction
      (narrow-to-region (region-beginning) (region-end))
      (replace-string "<" "&lt;" nil (point-min) (point-max))
      (replace-string ">" "&gt;" nil (point-min) (point-max))
      (if each
          (progn
            (goto-char (point-min))
            (while (< (point) (point-max))
              (funcall each)
              (forward-line))))
      (goto-char (point-max))
      (insert e)
      (goto-char (point-min))
      (insert b))))

(defun kill-prefix (c)
  (while (equal (char-after) c) (delete-char 1)))

(defun book-email-header ()
  (interactive)
  (beginning-of-line)
  (insert "<pre class=\"hdr\">\n")
  (let (value)
    (dotimes (number 4 value)
      (beginning-of-line)
      (save-excursion
        (delete-horizontal-space)
        (replace-string "<" "&lt;" nil (line-beginning-position) (line-end-position))
        (replace-string ">" "&gt;" nil (line-beginning-position) (line-end-position)))
      (insert "<p>")
      (end-of-line)
      (insert "</p>")
      (forward-line)))
    (beginning-of-line)
    (insert "</pre>\n"))


(defun book-quote ()
  (interactive)
  (book-wrap "<blockquote>\n<p>\n" "</p>\n</blockquote>\n"
             (lambda ()
               (kill-prefix ?#)
               (delete-horizontal-space)))
  (forward-line 2))


(defun book-p ()
  (interactive)
  (book-wrap "<p>\n" "</p>\n")
  (forward-line))

(defun book-paste-email ()
  (interactive)
  (save-excursion
    (let ((p (point)))
      (yank)
      (insert "\n\n<hr/>\n")
      (replace-string " " " " nil p (point))
      (goto-char p)
      (book-email-header)))
  (forward-line 6))

(defvar book-mode-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c h") 'book-email-header)
    (define-key map (kbd "C-c p") 'book-p)
    (define-key map (kbd "C-c q") 'book-quote)
    (define-key map (kbd "C-c y") 'book-paste-email)
    map)
   "Key mapping for book mode")

(define-minor-mode book-mode
  "Toggle book mode."
  :lighter " Book"
  :keymap book-mode-keymap)
