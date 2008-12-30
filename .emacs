;; Mac key bindings

; I like alt/option as meta
(setq mac-command-key-is-meta nil)
; And we'll use command within here
(setq mac-pass-command-to-system nil)
(global-set-key [?\A-x] 'kill-region)
(global-set-key [?\A-c] 'kill-ring-save)
(global-set-key [?\A-v] 'yank)
(global-set-key [?\A-q] 'save-buffers-kill-emacs)
(global-set-key [?\A-o] 'find-file)
(global-set-key [?\A-s] 'save-buffer)
(global-set-key [?\A-w] 'kill-buffer)
(global-set-key [?\A-z] 'undo)

(add-hook 'text-mode-hook
  (lambda()
    (auto-fill-mode)))

(add-hook 'sgml-mode-hook
  (lambda ()
    (setq tab-width 2)))

;; I haven't really figured out what I want to use for colors
; (set-background-color "black")
; (set-foreground-color "white")
; (set-cursor-color "white")
