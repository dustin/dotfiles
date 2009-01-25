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
(setq-default tab-width 4)
(setq-default c-basic-offset 4)

(add-to-list 'exec-path "/opt/local/bin")

(add-to-list 'load-path "~/elisp")
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

; caml modes

(add-to-list 'load-path "~/elisp/caml")

(setq auto-mode-alist
          (cons '("\\.ml[iyl]?$" .  caml-mode) auto-mode-alist))

(autoload 'caml-mode "ocaml" (interactive)
  "Major mode for editing Caml code." t)
(autoload 'camldebug "camldebug" (interactive) "Debug caml mode")

; http://www.emacswiki.org/emacs/DynamicAbbreviations
; http://trey-jackson.blogspot.com/2007/12/emacs-tip-5-hippie-expand.html
(global-set-key (kbd "M-/") 'hippie-expand)
(define-key minibuffer-local-map (kbd "C-<tab>") 'hippie-expand)

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

(require 'cl)

(defun dustin-libs-and-paths (l)
  "Load some libs and require something from them."
  (if l
      (progn
        (add-to-list 'load-path (concat "~/elisp/" (caar l)))
        (if (cadar l)
            (require (cadar l)))
        (dustin-libs-and-paths (cdr l)))))

(dustin-libs-and-paths
 '(
   ("muse/lisp")
   ("remember/" remember)
   ("planner/" planner)
   ("magit" magit)
   ("growl" growl)))

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
 '(erc-nick "dsal")
 '(jabber-account-list (quote (("dustin@sallings.org/emacs" (:network-server . "talk.google.com")))))
 '(org-agenda-files (quote ("~/todo.org"))))

(put 'upcase-region 'disabled nil)
; This is a little buggy in OS X.
(if (fboundp 'tooltip-mode)
    (tooltip-mode 0))
(put 'erase-buffer 'disabled nil)
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

; Local key binding lazily loaded for org-mode.
(add-hook 'org-mode-hook
          '(lambda ()
             (define-key org-mode-map "\C-ca"
               'org-agenda)))

(setq erc-autojoin-channels-alist
      '(("freenode.net" "#git" "#github" "#memcached" "#buildbot")))
(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT"))

(defun my-growl-erc-hook (match-type nick message)
  "Basic growl notification when someone says my nick in an irc channel."
  (and (eq match-type 'current-nick)
       (growl (concat "Your nick was mentioned on: "
                      (buffer-name (current-buffer))
                      "\n"
                      message))))

(add-hook 'erc-text-matched-hook 'my-growl-erc-hook)

(require 'dustin-timer)

;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.
(when
    (load
     (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))
