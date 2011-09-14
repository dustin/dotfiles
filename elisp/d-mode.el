;;; d-mode.el --- D code editing commands for Emacs

;;; Commentary:

;; D-Mode: Mode for editing DTrace D language.
;;
;; You can add the following to your .emacs:
;;
;; (autoload 'd-mode "d-mode" () t)
;; (add-to-list 'auto-mode-alist '("\\.d\\'" . d-mode))
;;
;; When loaded, runs all hooks from d-mode-hook
;; You may try
;;
;; (add-hook 'd-mode-hook 'imenu-add-menubar-index)
;; (add-hook 'd-mode-hook 'font-lock-mode)
;;
;; Alexander Kolbasov <akolb at sun dot com>
;;

;;
;; The D-mode inherits from C-mode.
;; It supports imenu and syntax highlighting.
;;

;; $Id: d-mode.el,v 1.3 2007/07/12 01:44:55 akolb Exp $

;;; Code:

(defvar d-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\e\C-a" 'd-beginning-of-function)
    (define-key map "\e\C-e" 'd-end-of-function)
    (define-key map "\e\C-h" 'd-mark-function)
    map)
  "Keymap used in D mode.")

(defvar d-mode-syntax-table
  (let ((st (make-syntax-table (standard-syntax-table))))
    (modify-syntax-entry ?/ "$" st)
    (modify-syntax-entry ?` "." st)
    (modify-syntax-entry ?: "." st)
    (modify-syntax-entry ?_ "w" st)
    (modify-syntax-entry ?$ "/" st)
    (modify-syntax-entry ?& "." st)
    (modify-syntax-entry ?* "." st)
    (modify-syntax-entry ?+ "." st)
    (modify-syntax-entry ?- "." st)
    (modify-syntax-entry ?< "." st)
    (modify-syntax-entry ?= "." st)
    (modify-syntax-entry ?> "." st)
    (modify-syntax-entry ?\\ "\\" st)
    (modify-syntax-entry ?/  ". 14" st)
    (modify-syntax-entry ?*  ". 23"   st)
    st)
  "Syntax table in use in `D-mode' buffers.")

;;
;; Show probes, pragmas and inlines in imenu
;;
(defvar d-imenu-generic-expression
  '(
    (nil "^\\s-*\\(\\sw+:.+\\)" 1 )
    (nil "\\s-*\\(BEGIN\\|END\\)" 1 )
    ("Pramgas" "^#pragma\\s-+D\\s-+\\(.+\\)" 1)
    ("Inlines" "\\s-*inline\\s-+\\(.*\\);" 1)
    )
  "Imenu generic expression for D mode.  See `imenu-generic-expression'.")

(defvar d-mode-hook nil
  "Hooks to run when entering D mode.")

;;
;; Definition of various DTrace keywords for font-lock-mode
;;
(defconst d-font-lock-keywords
  (eval-when-compile
    (list
     ;;
     ;; Function names.
     ;'("^\\(\\sw+\\):\\(\\sw+\\|:\\)?"
     ;   (1 font-lock-keyword-face) (2 font-lock-function-name-face nil t))
     ;;
     ;; Variable names.
     (cons (regexp-opt
	    '(
	      "egid" "euid" "gid" "pid" "pgid" "ppid" "projid" "sid"
	      "taskid" "uid") 'words)
	   'font-lock-variable-name-face)

     ;;
     ;; DTrace built-in variables
     ;;
     (cons (regexp-opt
	    '(
	      "NULL"
	      "arg0" "arg1" "arg2" "arg3" "arg4" "arg5" "arg6" "arg7"
	      "arg8" "arg9" 
	      "args" 
	      "caller" 
	      "chip"
	      "cpu"
	      "curcpu"
	      "curlwpsinfo" 
	      "curpsinfo" 
	      "curthread"
	      "cwd" 
	      "epid" 
	      "errno" 
	      "execname" 
	      "gid"
	      "id" 
	      "ipl"
	      "lgrp"
	      "pid"
	      "ppid"
	      "probefunc"
	      "probemod" 
	      "probename" 
	      "probeprov"
	      "pset"
	      "pwd" 
	      "root"
	      "self" 
	      "stackdepth"
	      "this"
	      "tid"
	      "timestamp"
	      "uid"
	      "uregs"
	      "vtimestamp"
	      "walltimestamp"
	      ) 'words)
	   'font-lock-constant-face)
     ;;
     ;; DTrace functions.
     ;;
     (list (regexp-opt
	    '(
	      "alloca"
	      "avg"
	      "basename"
	      "bcopy"
	      "cleanpath"
	      "commit"
	      "copyin" 
	      "copyinstr" 
	      "copyinto"
	      "copyout" 
	      "copyoutstr" 
	      "count"
	      "dirname"
	      "discard"
	      "exit"
	      "jstack"
	      "lquantize"
	      "max"
	      "min" 
	      "msgdsize"
	      "msgsize"
	      "mutex_owned" 
	      "mutex_owner" 
	      "mutex_type_adaptive"
	      "mutex_type_spin"
	      "offsetof" 
	      "printa"
	      "printf"
	      "progenyof"
	      "quantize"
	      "raise" 
	      "rand"
	      "rand" 
	      "rw_iswriter" 
	      "rw_read_held" 
	      "rw_write_held"
	      "speculate"
	      "speculation"
	      "stack"
	      "stop"
	      "stringof"
	      "strjoin"
	      "strlen"
	      "sum" 
	      "system"
	      "trace"
	      "tracemem" 
	      "trunc"
	      "ustack"
	      ) 'words)
	   1 'font-lock-builtin-face)
     ;;
     ;; Destructive actions
     ;;
     (list (regexp-opt
	    '(
	      "breakpoint"
	      "chill"
	      "panic"
	      ) 'words)
	   1 'font-lock-warning-face)
     ;;
     ;; DTrace providers
     ;;
     (regexp-opt
      '(
	"BEGIN"
	"END"
	"dtrace"
	"dtrace"
	"entry"
	"fasttrap"
	"fbt"
	"fpuinfo"
	"io"
	"lockstat"
	"mib"
	"pid"
	"plockstat"
	"proc"
	"profile"
	"return"
	"sched"
	"sdt"
	"syscall"
	"sysinfo"
	"tick"
	"vm"
	"vminfo"
	"vtrace"
	) 'words)))
  "Default expressions to highlight in D mode.")

;;;###autoload
(define-derived-mode d-mode c-mode "D"
  "Major mode for editing D code.
This is much like C mode.  Its keymap inherits from C mode's and it has the same
variables for customizing indentation.  It has its own abbrev table and its own
syntax table.
\\{d-mode-map}

Turning on D mode runs `d-mode-hook'."
  (setq imenu-generic-expression d-imenu-generic-expression)
  (setq font-lock-defaults '(d-font-lock-keywords nil nil ((?_ . "w")))))

(defun d-beginning-of-function (&optional arg)
  "Move backward to next beginning-of-function, or as far as possible.
With argument, repeat that many times; negative args move forward.
Returns new value of point in all cases."
  (interactive "p")
  (or arg (setq arg 1))
  (if (< arg 0) (forward-char 1))
  (and (/= arg 0)
       (re-search-backward "^[a-z_]+:.*$"
			   nil 'move arg)
       (goto-char (1- (match-end 0))))
  (beginning-of-line))

;; note: this routine is adapted directly from emacs perl-mode.el.
;; no bugs have been removed :-)
(defun d-end-of-function (&optional arg)
  "Move forward to next end-of-function.
The end of a function is found by moving forward from the beginning of one.
With argument, repeat that many times; negative args move backward."
  (interactive "p")
  (or arg (setq arg 1))
  (let ((first t))
    (while (and (> arg 0) (< (point) (point-max)))
      (let ((pos (point)) npos)
	(while (progn
		(if (and first
			 (progn
			  (forward-char 1)
			  (d-beginning-of-function 1)
			  (not (bobp))))
		    nil
		  (or (bobp) (forward-char -1))
		  (d-beginning-of-function -1))
		(setq first nil)
		(forward-list 1)
		(skip-chars-forward " \t")
		(if (looking-at "[#\n]")
		    (forward-line 1))
		(<= (point) pos))))
      (setq arg (1- arg)))
    (while (< arg 0)
      (let ((pos (point)))
	(d-beginning-of-function 1)
	(forward-sexp 1)
	(forward-line 1)
	(if (>= (point) pos)
	    (if (progn (d-beginning-of-function 2) (not (bobp)))
		(progn
		  (forward-list 1)
		  (skip-chars-forward " \t")
		  (if (looking-at "[#\n]")
		      (forward-line 1)))
	      (goto-char (point-min)))))
      (setq arg (1+ arg)))))

(defun d-mark-function ()
  "Put mark at end of D function, point at beginning."
  (interactive)
  (push-mark (point))
  (d-end-of-function)
  (push-mark (point))
  (d-beginning-of-function)
  (backward-paragraph))


(provide 'd-mode)

;;; d-mode.el ends here
