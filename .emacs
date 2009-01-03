; I like paren matching
(show-paren-mode t)

; Keep the weird, not-very mac-like bindings.
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'meta)

(add-hook 'text-mode-hook
  (lambda()
    (auto-fill-mode)))

(add-hook 'sgml-mode-hook
  (lambda ()
    (setq tab-width 2)))

(add-to-list 'load-path "/usr/local/share/emacs/site-lisp")
(require 'magit)

(add-to-list 'exec-path "/opt/local/bin")

(add-to-list 'load-path "~/.emacs.d")
(autoload 'markdown-mode "markdown-mode.el"
   "Major mode for editing Markdown files" t)
(setq auto-mode-alist
   (cons '("\\.markdown" . markdown-mode) auto-mode-alist))

(require 'haml-mode nil 't)
(add-to-list 'auto-mode-alist '("\\.haml$" . haml-mode))

(require 'sass-mode nil 't)
(add-to-list 'auto-mode-alist '("\\.sass$" . sass-mode))

; http://www.emacswiki.org/emacs/DynamicAbbreviations
(global-set-key (kbd "C-<tab>") 'dabbrev-expand)
(define-key minibuffer-local-map (kbd "C-<tab>") 'dabbrev-expand)

; Yay, highlighting
(transient-mark-mode 't)

; Ido
(ido-mode t)
(setq ido-enable-flex-matching t)

;; Do I want this to be like textmate?
; (global-set-key "\M-t" 'find-tag)

;; I haven't really figured out what I want to use for colors
; (set-background-color "black")
; (set-foreground-color "white")
; (set-cursor-color "white")
