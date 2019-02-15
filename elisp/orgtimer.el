(require 'chronos)

(defvar orgtimer-duration "60"
  "The amount of time (in minutes) an item should be run.")

(defvar orgtimer-interval "15"
  "The amount of time (in minutes) until the next item should start.")

(defun orgtimer-timeme ()
  "Start a timer to 'up' the current heading after orgtimer-duration,
and 'down' the next heading after orgtimer-interval."

  (interactive)
  (let ((iname (nth 4 (org-heading-components))))
    (chronos-add-timer orgtimer-duration (concat "up " iname) nil)
    ;; (org-set-property "STARTED" (format-time-string "%Y-%m-%d %H:%M:%S"))
    (run-hook-with-args 'orgtimer-timeme-hooks iname))
  (org-todo "INPROGRESS")
  (outline-next-visible-heading 1)
  (chronos-add-timer orgtimer-interval (concat "down " (nth 4 (org-heading-components))) nil))

(defun orgtimer-complete ()
  "Finish him!"
  (interactive)
  (let ((iname (nth 4 (org-heading-components))))
    (run-hook-with-args 'orgtimer-complete-hooks iname))
  (org-todo "DONE"))

(defvar orgtimer-mode-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c t") 'orgtimer-timeme)
    (define-key map (kbd "C-c d") 'orgtimer-complete)
    map)
  "Key mapping for orgtimer mode.")

(define-minor-mode orgtimer-mode
  "orgtimer mode for timing stuff."
  :lighter " orgtmr"
  :keymap orgtimer-mode-keymap
  (make-variable-buffer-local 'orgtimer-duration)
  (make-variable-buffer-local 'orgtimer-interval)
  (chronos-initialize))

(provide 'orgtimer)
