; I like paren matching
(show-paren-mode t)

; Keep the weird, not-very mac-like bindings.
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'meta)

(condition-case nil
    (when
        (load
         (expand-file-name "~/.emacs.d/elpa/package.el"))
      (package-initialize))
  (file-error (message "Failed to load package stuff.")))

(require 'cl)

(if (fboundp 'package-initialize)
    (progn
      (package-initialize)
      (add-to-list 'package-archives
                   '("melpa" . "http://melpa.milkbox.net/packages/") t)

      ;; See package-activated-list for what's installed now
      (let* ((wanted-packages
              `(all auctex auto-complete caml
               color-theme company csv-mode
               dockerfile-mode dot-mode ess fuzzy
               go-autocomplete go-eldoc go-mode graphviz-dot-mode
               haml-mode haskell-mode htmlize imenu-anywhere
               impatient-mode js2-mode json-mode kill-ring-search
               lua-mode magit markdown-mode muse oauth popup
               pov-mode sass-mode simple-httpd
               xcscope yaml-mode yasnippet
               zen-and-art-theme))
            (all-installed
             (loop for p in wanted-packages
                   when (not (package-installed-p p)) do (return nil)
                   finally (return t))))

        (unless all-installed
          ;; check for new packages (package versions)
          (message "%s" "Emacs Prelude is now refreshing its package database...")
          (package-refresh-contents)
          (message "%s" " done.")
          ;; install the missing packages
          (dolist (p wanted-packages)
            (when (not (package-installed-p p))
              (package-install p)))))))

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

(add-to-list 'load-path "~/elisp")

(add-to-list 'auto-mode-alist '("\\.dot\\'" . graphviz-dot-mode))

(add-to-list 'auto-mode-alist '("\\.tac" . python-mode))

(add-to-list 'auto-mode-alist '("\\.pde" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.ino" . c++-mode))

(add-to-list 'auto-mode-alist '("\\.rake" . ruby-mode))

(add-to-list 'auto-mode-alist '("\\.sass$" . sass-mode))

(add-to-list 'auto-mode-alist '("\\.app" . erlang-mode))

; caml modes

(add-to-list 'auto-mode-alist '("\\.ml[iyl]?$" . caml-mode))

(autoload 'caml-mode "ocaml" (interactive)
  "Major mode for editing Caml code." t)
(autoload 'camldebug "camldebug" (interactive) "Debug caml mode")

(autoload (quote go-mode) "go-mode" "\
Major mode for editing Go source text.

This provides basic syntax highlighting for keywords, built-ins,
functions, and some types.  It also provides indentation that is
\(almost) identical to gofmt.

\(fn)" t nil)

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

(defun dustin-libs-and-paths (l)
  "Load some libs and require something from them."
  (if l
      (progn
        (add-to-list 'load-path (concat "~/elisp/" (caar l)))
        (if (cadar l)
            (require (cadar l)))
        (dustin-libs-and-paths (cdr l)))))

(autoload 'slime "slime")

(global-set-key (kbd "C-c m") 'magit-status)

(server-start)

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

(defmacro safe-wrap (fn &rest clean-up)
  `(unwind-protect
       (let (retval)
         (condition-case ex
             (setq retval (progn ,fn))
           ('error
            (message (format "Caught exception: [%s]" ex))
            (setq retval (cons 'exception (list ex)))))
         retval)
     ,@clean-up))

(safe-wrap (load (expand-file-name "~/elisp/org-exports.el")))

(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT"))

(add-hook 'erc-mode-hook (lambda () (auto-fill-mode 0)))

; tramp is neat
(setq tramp-default-method "ssh")
(setq tramp-auto-save-directory "~/tmp/tramp/")
(setq tramp-chunksize 2000)

(require 'dustin-timer)
; Keep things tidy
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
    (indent-region (point-min) (point-max))
    (delete-trailing-whitespace)))

(add-hook 'dustin-periodic-task-hooks 'dustin-cleanup-rnc-crap)

(autoload 'revbufs "revbufs" (interactive) "Buffer reverter.")

(defun my-go-mode-hook ()
  (add-hook 'before-save-hook 'gofmt-before-save)
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go vet && go test -cover -v && go build -v"))
  (setq tab-width 8 indent-tabs-mode 1)
  (local-set-key (kbd "M-.") 'godef-jump))
(add-hook 'go-mode-hook 'my-go-mode-hook)
(add-hook 'go-mode-hook 'go-eldoc-setup)

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

(global-set-key "\M-\C-y" 'kill-ring-search)

;; My JSON tools.
(defun format-json-on-point (start end)
  "Clean up the JSON between two points"
  (save-excursion
    (shell-command-on-region start end "python -mjson.tool" 'nil t)))

(defun format-json ()
  "Clean up JSON in the current buffer."
  (interactive)
  (format-json-on-point (point-min) (point-max))
  (delete-trailing-whitespace))

(defun format-json-region ()
  "Clean up JSON in a selected region"
  (interactive)
  (format-json-on-point (region-beginning) (region-end))
  (delete-trailing-whitespace))

(defun format-json-setup-save-hook ()
  "Automatically clean up JSON before saving."
  (interactive)
  (message "Set up format-json hook for this buffer.")
  (add-hook 'before-save-hook 'format-json nil t))

;; https://code.google.com/p/ergoemacs/source/browse/packages/xah-insert-random-id.el

(defun random-selection (alphabet n)
  "Get a random selection of n characters from the given alphabet."
  (concat (let ((rv nil) (alen (length alphabet)))
    (dotimes (i n rv)
      (setq rv (cons (elt alphabet (random alen)) rv))))))

(defun insert-random-number (φcount)
  "Insert φcount of random digits.
φcount default to 5"
  (interactive "P")
  (insert (random-selection "1234567890"
                            (if (numberp φcount) (abs φcount) 5))))

(setq trash-directory
      (loop for p in '("~/.Trash" "~/.trash" "~/trash")
            when (file-exists-p p) do (return p)))

;; My local stuff.
(if (file-readable-p "~/.emacs.local")
    (load
     (expand-file-name "~/.emacs.local")))
