; I like paren matching
(show-paren-mode t)

(add-hook 'text-mode-hook
  (lambda()
    (auto-fill-mode)))

(add-hook 'sgml-mode-hook
  (lambda ()
    (setq tab-width 2)))

(add-to-list 'load-path "/usr/local/share/emacs/site-lisp")
(require 'magit)

;; I haven't really figured out what I want to use for colors
; (set-background-color "black")
; (set-foreground-color "white")
; (set-cursor-color "white")
