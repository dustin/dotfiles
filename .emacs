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

;; ; I want html to use nxml mode
;; (setq auto-mode-alist
;;           (cons '("\\.html$" .  nxml-mode) auto-mode-alist))

(add-to-list 'load-path "~/elisp")
(autoload 'markdown-mode "markdown-mode.el"
   "Major mode for editing Markdown files" t)
(setq auto-mode-alist
   (cons '("\\.markdown" . markdown-mode) auto-mode-alist))
(setq auto-mode-alist
   (cons '("\\.md" . markdown-mode) auto-mode-alist))

(setq auto-mode-alist
   (cons '("\\.tac" . python-mode) auto-mode-alist))

(autoload 'haml-mode "haml-mode")
(add-to-list 'auto-mode-alist '("\\.haml$" . haml-mode))

(autoload 'sass-mode "sass-mode")
(add-to-list 'auto-mode-alist '("\\.sass$" . sass-mode))

(autoload 'yaml-mode "yaml-mode")
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

; caml modes

(add-to-list 'load-path "~/elisp/caml")

(setq auto-mode-alist
          (cons '("\\.ml[iyl]?$" .  caml-mode) auto-mode-alist))

(autoload 'caml-mode "ocaml" (interactive)
  "Major mode for editing Caml code." t)
(autoload 'camldebug "camldebug" (interactive) "Debug caml mode")

; haskell mode
(add-to-list 'load-path "~/elisp/haskell-mode")
(setq auto-mode-alist
      (append auto-mode-alist
              '(("\\.[hg]s$"  . haskell-mode)
                ("\\.hi$"     . haskell-mode)
                ("\\.l[hg]s$" . literate-haskell-mode))))

(autoload 'haskell-mode "haskell-mode"
  "Major mode for editing Haskell scripts." t)
(autoload 'literate-haskell-mode "haskell-mode"
  "Major mode for editing literate Haskell scripts." t)

;; Javascript
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; D
(autoload 'd-mode "d-mode" "Major mode for editing D code." t)
(add-to-list 'auto-mode-alist '("\\.d[i]?\\'" . d-mode))

(setq auto-mode-alist (cons '("\\.lua$" . lua-mode) auto-mode-alist))
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)

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

; If there's a battery mode, display it.
(if (fboundp 'display-battery-mode)
    (display-battery-mode t))

; Look.  I can read email *sigh*
(setq imap-ssl-program "openssl s_client -tls1 -connect %s:%p")
;; (setq gnus-select-method '(nnimap "spy.net"
;;                                   (nnimap-address "imap.west.spy.net")
;;                                   (nnimap-stream ssl)))

(setq gnus-select-method '(nntp "news.giganews.com"))

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
   ("growl" growl)
   ("slime")))

(autoload 'slime "slime")

(global-set-key (kbd "C-c m") 'magit-status)

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
 '(org-agenda-files (quote ("~/Dropbox/work/todo.org"))))

(put 'upcase-region 'disabled nil)
; This is a little buggy in OS X.
(if (fboundp 'tooltip-mode)
    (tooltip-mode 0))
; Give me more of my ubuntu window.
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode 0))

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

(setq org-log-done 'time)
(setq org-todo-keywords
      '((sequence "TODO(t)" "INPROGRESS(i)" "|" "DONE(d)")))
(setq org-refile-use-outline-path t)
(setq org-refile-targets '((nil . (:maxlevel . 3))))

(if (fboundp 'org-remember-insinuate)
    (org-remember-insinuate)
  (progn
    (setq remember-annotation-functions '(org-remember-annotation))
    (setq remember-handler-functions '(org-remember-handler))
    (add-hook 'remember-mode-hook 'org-remember-apply-template)))

(setq org-directory "~/Dropbox/work")
(setq org-default-notes-file (concat org-directory "/todo.org"))
(define-key global-map "\C-cr" 'org-remember)

(setq erc-autojoin-channels-alist
      '(("freenode.net" "#github" "#memcached" "#buildbot" "#slumbrparty")))
(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT"))

(add-hook 'erc-mode-hook (lambda () (auto-fill-mode 0)))

(defun my-growl-erc-hook (match-type nick message)
  "Basic growl notification when someone says my nick in an irc channel."
  (and (eq match-type 'current-nick)
       (growl (concat "Your nick was mentioned on: "
                      (buffer-name (current-buffer))
                      "\n"
                      message))))

(add-hook 'erc-text-matched-hook 'my-growl-erc-hook)

; tramp is neat
(setq tramp-default-method "ssh")
(setq tramp-auto-save-directory "~/tmp/tramp/")
(setq tramp-chunksize 2000)

(require 'dustin-timer)
; Keep things tidy
; Keep old buffers away aggressively
(setq clean-buffer-list-delay-special 300)
(setq clean-buffer-list-delay-general 1)
; Run the periodic tasks hourly (keep in mind it only runs when idle)
(dustin-schedule-periodic (* 60 60))

(defun dustin-cleanup-rnc-crap ()
  "Remove all of the RNC input buffers that litter up my world."

  (interactive)
  (dolist (buffer (buffer-list))
    (if (string-match "RNC Input" (buffer-name buffer))
        (progn
          (message "Killing %s" (buffer-name buffer))
          (kill-buffer buffer)))))

(defun reformat-file ()
  "Reformat a file using tab expansion, buffer marking, etc..."
  (interactive)
  (save-excursion
    ; Probably best to just let the buffer set it itself
    ; (setq tab-width 4)
    (untabify (point-min) (point-max))
    (indent-region (point-min) (point-max))
    (delete-trailing-whitespace)))

(add-hook 'dustin-periodic-task-hooks 'dustin-cleanup-rnc-crap)

;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.
(condition-case nil
    (when
        (load
         (expand-file-name "~/.emacs.d/elpa/package.el"))
      (package-initialize))
  (file-error (message "Failed to load package stuff.")))

(autoload 'revbufs "revbufs" (interactive) "Buffer reverter.")

(if (file-readable-p "~/.emacs.local")
    (load
     (expand-file-name "~/.emacs.local")))
