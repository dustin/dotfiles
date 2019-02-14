(require 'chronos)

(defvar orgtimer-duration "60"
  "The amount of time (in minutes) an item should be run.")

(defvar orgtimer-interval "15"
  "The amount of time (in minutes) until the next item should start.")

(defun orgtimer-timeme ()
  "Start a timer to 'up' the current heading after orgtimer-duration,
and 'down' the next heading after orgtimer-interval."

  (interactive)
  (chronos-add-timer orgtimer-duration (concat "up " (nth 4 (org-heading-components))) nil)
  ;; (org-set-property "STARTED" (format-time-string "%Y-%m-%d %H:%M:%S"))
  (org-todo "INPROGRESS")
  (outline-next-visible-heading 1)
  (chronos-add-timer orgtimer-interval (concat "down " (nth 4 (org-heading-components))) nil))

(defvar orgtimer-mode-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c t") 'orgtimer-timeme)
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
