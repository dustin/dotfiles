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

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default c-basic-offset 4)
(c-set-offset 'innamespace 0)

(add-to-list 'exec-path "/opt/local/bin")

;; ; I want html to use nxml mode
;; (setq auto-mode-alist
;;           (cons '("\\.html$" .  nxml-mode) auto-mode-alist))

(autoload 'cheat "cheat" "Cheater." t)

(autoload 'django-html-mode "django-html-mode.el"
   "Major mode for editing django template files" t)

(add-to-list 'load-path "~/elisp")
(autoload 'markdown-mode "markdown-mode.el"
   "Major mode for editing Markdown files" t)
(setq auto-mode-alist
   (cons '("\\.markdown" . markdown-mode) auto-mode-alist))
(setq auto-mode-alist
   (cons '("\\.md" . markdown-mode) auto-mode-alist))

(autoload 'graphviz-dot-mode "graphviz-dot-mode.el"
  "Major mode for graphviz")
(add-to-list 'auto-mode-alist '("\\.dot\\'" . graphviz-dot-mode))

(autoload 'wikipedia-mode
  "wikipedia-mode.el"
  "Major mode for editing documents in Wikipedia markup." t)
(setq auto-mode-alist
      (cons '("\\.wiki\\'" . wikipedia-mode) auto-mode-alist))

(autoload 'protobuf-mode "protobuf-mode.el" "Probuffer mode.")
(setq auto-mode-alist
      (cons '("\\.proto\\'" . protobuf-mode) auto-mode-alist))

(setq auto-mode-alist
   (cons '("\\.tac" . python-mode) auto-mode-alist))

(setq auto-mode-alist
      (cons '("\\.pde" . c++-mode) auto-mode-alist))
(setq auto-mode-alist
      (cons '("\\.ino" . c++-mode) auto-mode-alist))

(setq auto-mode-alist
   (cons '("\\.rake" . ruby-mode) auto-mode-alist))

(autoload 'haml-mode "haml-mode")
(add-to-list 'auto-mode-alist '("\\.haml$" . haml-mode))

(autoload 'sass-mode "sass-mode")
(add-to-list 'auto-mode-alist '("\\.sass$" . sass-mode))

(autoload 'yaml-mode "yaml-mode")
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

(autoload 'feature-mode "feature-mode" "Mode for editing cucumber files" t)
(add-to-list 'auto-mode-alist '("\.feature$" . feature-mode))

(autoload 'cflow-mode "cflow-mode")
(setq auto-mode-alist (append auto-mode-alist
                              '(("\\.cflow$" . cflow-mode))))

(setq auto-mode-alist
   (cons '("\\.app" . erlang-mode) auto-mode-alist))

; caml modes

(add-to-list 'load-path "~/elisp/caml")

(setq auto-mode-alist
          (cons '("\\.ml[iyl]?$" .  caml-mode) auto-mode-alist))

(autoload 'caml-mode "ocaml" (interactive)
  "Major mode for editing Caml code." t)
(autoload 'camldebug "camldebug" (interactive) "Debug caml mode")

(autoload 'twitter-get-friends-timeline "twitter" nil t)
(autoload 'twitter-status-edit "twitter" nil t)
(global-set-key "\C-xt" 'twitter-get-friends-timeline)
(add-hook 'twitter-status-edit-mode-hook 'longlines-mode)

(autoload (quote go-mode) "go-mode" "\
Major mode for editing Go source text.

This provides basic syntax highlighting for keywords, built-ins,
functions, and some types.  It also provides indentation that is
\(almost) identical to gofmt.

\(fn)" t nil)

(add-to-list (quote auto-mode-alist) (cons "\\.go$" (function go-mode)))

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



(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))

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

(setq gnus-select-method '(nntp "news.gmane.org"))

(setq user-mail-address "dustin@spy.net")

(setq gnus-agent t)

(setq gnus-posting-styles
      '((".*"
         (name "Dustin Sallings")
         (signature "dustin"))
        ("gmane.*"
         (address "dsallings@gmail.com"))
        ("gmane.comp.db.couchdb.*"
         (address "dustin@spy.net"))
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
   ("coffee" coffee-mode)
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

(setq custom-file "~/.emacs-custom.el")
(load custom-file 'noerror)

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

; use custom org-mode if I have it
(setq load-path (cons "~/prog/eprojects/org-mode/lisp" load-path))

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

(setq org-directory "~/org")
(setq org-default-notes-file (concat org-directory "/todo.org"))
(define-key global-map "\C-cr" 'org-remember)

(load
 (expand-file-name "~/elisp/org-export-generic.el"))

; Export for google code
(org-set-generic-type
 "googlecode"
 '(:file-suffix        	    ".txt"
     :key-binding                   ?G

     :header-prefix            	    ""
     :header-suffix            	    ""

     :title-format             	    "= %s =\n"

     :date-export        	        t
     :date-format                   "Date: %s\n"

     :toc-export                    nil

     :body-header-section-numbers   nil
     :body-section-prefix           "\n"

     :body-section-header-prefix    ("== " "=== "
				     "==== " "===== " "====== ")
     :body-section-header-suffix    (" ==\n" " ===\n"
				     " ====\n" " =====\n" " ======\n")

     :body-line-export-preformated  t          ;; yes/no/maybe???
     :body-line-format              "%s\n"
     :body-line-wrap                75

     :body-line-fixed-prefix       "{{{\n"
     :body-line-fixed-format       "%s\n"
     :body-line-fixed-suffix       "}}}\n"

     :body-list-format              "  * %s\n"
     :body-number-list-format       "  # %s\n"

     :body-bullet-list-prefix       ("* " "** " "*** " "**** " "***** ")
   ))

; Export for twiki
(org-set-generic-type
 "twiki"
 '(:file-suffix        	    ".txt"
     :key-binding                   ?T

     :header-prefix            	    ""
     :header-suffix            	    ""

     :title-format             	    ""

     :date-export        	        t
     :date-format                   ""

     :toc-export                    nil

     :body-header-section-numbers   nil
     :body-section-prefix           "\n"

     :body-section-header-prefix    ("---+ " "---++ "
				     "---+++ " "---++++ " "---+++++ ")
     :body-section-header-suffix    ("\n" "\n" "\n" "\n" "\n")

     :body-line-export-preformated  t          ;; yes/no/maybe???
     :body-line-format              "%s\n"
     :body-line-wrap                75

     :body-line-fixed-prefix       "<verbatim>\n"
     :body-line-fixed-format       "%s\n"
     :body-line-fixed-suffix       "</verbatim>\n"

     :body-list-format              "%s\n"
     :body-number-list-format       "  # %s\n"

     :body-bullet-list-prefix       ("   * " "      * " "         * "
                                     "            * " "               * ")
   ))

; Export for
(org-set-generic-type
 "confluence"
 '(:file-suffix        	    ".txt"
     :key-binding                   ?C

     :header-prefix            	    ""
     :header-suffix            	    ""

     :title-format             	    "{panel:bgColor=#f0f0f0}\n{toc:outline=true|style=none|indent=10px}\n{panel}\n\n"

     :date-export        	        t
     :date-format                   ""

     :toc-export                    nil

     :body-header-section-numbers   nil
     :body-section-prefix           ""

     :body-section-header-prefix    ("h1. " "h2. " "h3. " "h4. " "h5. ")
     :body-section-header-suffix    ("\n" "\n" "\n" "\n" "\n")

     :body-line-export-preformated  t          ;; yes/no/maybe???
     :body-line-format              "%s\n"
     :body-line-wrap                75

     :body-line-fixed-prefix       "{{"
     :body-line-fixed-format       "%s"
     :body-line-fixed-suffix       "}}"

     :body-list-format              "%s\n"
     :body-number-list-format       "# %s\n"

     :body-bullet-list-prefix       ("* " "** " "*** " "**** " "***** ")
   ))

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

(if (fboundp 'package-initialize)
    (progn
      (package-initialize)
      (add-to-list 'package-archives
                   '("marmalade" . "http://marmalade-repo.org/packages/"))))

(autoload 'revbufs "revbufs" (interactive) "Buffer reverter.")

(defun my-go-mode-hook ()
  (add-hook 'before-save-hook 'gofmt-before-save)
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet"))
  (setq tab-width 8 indent-tabs-mode 1))
(add-hook 'go-mode-hook 'my-go-mode-hook)

(require 'color-theme)
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     (color-theme-taming-mr-arneson)))

;; Keep .gitignore files sorted.
(defun dustin-sort-lines-hook ()
  "Sort all of the lines in a file before saving."
    (save-excursion
      (sort-lines nil (point-min) (point-max))))

(defun dustin-maybe-visiting-gitignore-hook ()
  "Set up a line sorting hook if visiting a file that needs sorting."
  (if (and (buffer-file-name)
           (equal ".gitignore" (file-name-nondirectory (buffer-file-name))))
      (add-hook 'before-save-hook 'dustin-sort-lines-hook nil t)))

(add-hook 'find-file-hook 'dustin-maybe-visiting-gitignore-hook)

; http://www.ogre.com/node/447
(defun git-grep (search)
  "git-grep the current git repo"
  (interactive (list (completing-read "Search for: " nil nil nil (current-word))))
  (grep-find (concat "git --no-pager grep -n "
                     search " `git rev-parse --show-toplevel`")))

(defun my-general-programming-hooks ()
  ; Stupid trailing whitespace.
  (setq show-trailing-whitespace t)
  )

(setq my-programming-modes-list
      '(c-mode c++-mode erlang-mode markdown-mode protobuf-mode
               python-mode haml-mode sass-mode yaml-mode feature-mode
               wikipedia-mode caml-mode go-mode haskell-mode js2-mode
               d-mode lua-mode lisp-mode emacs-lisp-mode))

(dolist (mode my-programming-modes-list)
  (add-hook
   (intern (concatenate 'string (symbol-name mode) "-hook"))
   'my-general-programming-hooks))

;; OK, trying out some fkey bindings...
(define-key global-map [f5] 'recompile)

;; My local stuff.
(if (file-readable-p "~/.emacs.local")
    (load
     (expand-file-name "~/.emacs.local")))
