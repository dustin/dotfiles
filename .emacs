; I like paren matching
(show-paren-mode t)

; Stupid trailing whitespace.
(setq-default show-trailing-whitespace t)

; Keep the weird, not-very mac-like bindings.
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'meta)

(add-hook 'text-mode-hook
  (lambda()
    (auto-fill-mode)))

(add-hook 'sgml-mode-hook
  (lambda ()
    (setq tab-width 2)))

(setq-default indent-tabs-mode nil)

(add-to-list 'load-path "/usr/local/share/emacs/site-lisp")
(require 'magit)

(add-to-list 'exec-path "/opt/local/bin")

(add-to-list 'load-path "~/.emacs.d")
(autoload 'markdown-mode "markdown-mode.el"
   "Major mode for editing Markdown files" t)
(setq auto-mode-alist
   (cons '("\\.markdown" . markdown-mode) auto-mode-alist))

(setq auto-mode-alist
   (cons '("\\.tac" . python-mode) auto-mode-alist))

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

; Time?  Why not
(setq display-time-24hr-format t)
(display-time-mode t)

; Look.  I can read email *sigh*
(setq imap-ssl-program "openssl s_client -tls1 -connect %s:%p")
(setq gnus-select-method '(nnimap "spy.net"
                                  (nnimap-address "imap.west.spy.net")
                                  (nnimap-stream ssl)))

(setq user-mail-address "dustin@spy.net")

(setq gnus-posting-styles
      '((".*"
         (name "Dustin Sallings")
         ("X-URL" "http://bleu.west.spy.net/~dustin/"))
        ("work"
         (address user-mail-address))
        ("spy.net"
         (address user-mail-address))))

(setq inhibit-startup-message t)

(server-start)

;; Do I want this to be like textmate?
; (global-set-key "\M-t" 'find-tag)

;; I haven't really figured out what I want to use for colors
; (set-background-color "black")
; (set-foreground-color "white")
; (set-cursor-color "white")
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(jabber-account-list (quote (("dustin@sallings.org/emacs"
				(:network-server . "talk.google.com"))))))

(put 'upcase-region 'disabled nil)
; This is a little buggy in OS X.
(tooltip-mode 0)