" Copyright (c) 2002  Dustin Sallings
"
" $Id: .vimrc,v 1.18 2003/10/08 22:35:49 dustin Exp $
"
if has("terminfo")
  set t_Co=8
  set t_Sf=[3%p1%dm
  set t_Sb=[4%p1%dm
else
  set t_Co=8
  set t_Sf=[3%dm
  set t_Sb=[4%dm
endif

" General settings for other scripts that can't be defined as part of the
" general file loading stuff.
let java_allow_cpp_keywords=1
let java_highlight_functions="style"
let java_highlight_java_lang=1
" let java_space_errors=1
let java_highlight_debug=1
let java_highlight_functions=1

" Broken ass linux terminal
if $OSTYPE == "linux"
	set t_kb=
	fixdel
endif

" :set bg=dark
" if has("gui_running")
if has("gui")
	hi Normal guifg=white guibg=black
endif

" What to display in list mode
set listchars=tab:>-,trail:-,eol:$

syntax on

set ts=4
" wrapmargin is deprecated in favor of textwidth
set textwidth=79
" set wm=5
set ai
set shiftwidth=4
set sm

" Lots of buffers
set hid

" Stuff to gignore
set wildignore=*.o,*.obj,*.bak,*.exe,*.class

" Some neat options
set nocompatible    " Use Vim defaults (much better!)
set nobackup        " keep a backup file
set viminfo='1000,f1,\"250    " Vim info.  Remember the Alamo

" Neat things for searching patterns
set incsearch
set hlsearch

set dir=~/tmp

" Automatically locate template files
au BufNewFile * call NewFileStuff(expand("%:e"))
" Automatically load scripts for the specific file type.
au BufNewFile,BufReadPost * call GeneralFileStuff(expand("%:e"))

" :au BufNewFile,BufReadPost *.inc set syntax=html
au BufNewFile,BufReadPost *.xtp setlocal filetype=jsp
au BufNewFile,BufReadPost *.nqc setlocal filetype=c
au BufNewFile,BufReadPost *.xsl setlocal filetype=xml
au BufNewFile,BufReadPost *.zul setlocal filetype=xml
" :au BufNewFile,BufReadPost *.xinc setlocal syntax=xml
au BufNewFile,BufReadPost *.mocha setlocal filetype=java
au BufNewFile,BufReadPost *.ojava setlocal filetype=java
au BufNewFile,BufReadPost *.jad setlocal filetype=java
au BufNewFile,BufReadPost *.tld setlocal filetype=xml
au BufNewFile,BufReadPost *.spt setlocal filetype=sql
au BufNewFile,BufReadPost *.hrl setlocal filetype=erlang

" Python and jython stuff.  I'm going to go ahead and uses spaces in python
au BufNewFile,BufReadPost *.jy setlocal filetype=python expandtab
au BufNewFile,BufReadPost *.tac setlocal filetype=python expandtab
au BufNewFile,BufReadPost *.py setlocal expandtab
au BufNewFile,BufReadPost *.rb setlocal expandtab ts=2

" Scheme stuff
au BufNewFile,BufReadPost *.scm setlocal lisp

" OCaml
au BufNewFile *.mli call LoadTemplate("ml")

" For any new file, try to set a uuidgen value if there is one
au BufNewFile * call SetArchTag()

" SPTs
au BufNewFile *.spt call NewJavaFile()

" Java stuff
" How to make a new java file
au BufNewFile *.java call NewJavaFile()
au BufNewFile *.ojava call NewJavaFile()

" Rewrite CLASSNAME to the current filename without the extension
fun NewJavaFile()
	if(search("CLASSNAME"))
		exe "%s/CLASSNAME/" . expand("%:t:r") . "/g"
	endif
	if(search("PACKAGE"))
		let dirname = expand("%:p:h")
		let packagestart = matchend(dirname, ".*/\\\(src\\\|java\\\|tests\\\)/")
		if packagestart != -1
			let packagepath = strpart(dirname, packagestart)
			let packagename = substitute(packagepath, "/", ".", "g")
			exe "%s/PACKAGE/" . packagename . "/g"
		endif
	endif
endfun

" Arch integration
fun GetArchTag()
	let uuid=system("uuidgen")
	if v:shell_error
		" no uuidgen, use the username and the date
		let uuid=expand("$USER") . "_" . strftime('%Y%m%dT%H%M%S') . "_" . expand("$$")
	else
		" Get rid of characters we don't want from the uuid
		let uuid = substitute(uuid, "[^-A-Z0-9]", "", "g")
	endif
	return uuid
endfun

fun SetArchTag()
	" Replace any UUIDGEN in the template
	if search("@UUIDGEN@")
		exec "%s/@UUIDGEN@/" . GetArchTag() . "/g"
	endif
endfun

fun AddArchTag()
	call append(line("."), "arch-tag: " . GetArchTag())
	join
endfun

" Mappings for stuff I do a lot in arch
map <Leader>at :call AddArchTag()

" Tags
set tags=./tags,tags,~/.twtags

" Compliance with coding standards.
au BufNewFile,BufReadPost *.jsp  setlocal ts=2
au BufNewFile,BufReadPost *.html  setlocal ts=2
au BufNewFile,BufReadPost *.xml  setlocal ts=2

" Perforce stuff
map <Leader>o :!p4 edit %:setlocal noro
map <Leader>pa :!p4 add %

map <Leader>n :nohls

" Get rid of that stupid control a thing.
imap <C-A> <ESC>a

set encoding=utf-8

" ----------------
" Templating
" ----------------

let TmplDir="~/prog/snippets/java/templates"

function LoadTmpl(tempname)
	" Erase buffer
	exe "%del"
	" Load the template
	exe ":read " . g:TmplDir . "/" . a:tempname
	" Perform substitutions
	call NewJavaFile()
endfun

fun ListTmpl(A,L,P)
	let newd=expand(g:TmplDir)
	exe ":cd " . newd
	let rv=glob("*")
	cd -
	return rv
endfun

" Define the UseTemplate command for arbitrary templates
command -nargs=1 -complete=custom,ListTmpl UseTemplate call LoadTmpl("<args>")

" ----------------
" End of templating
" ----------------

" --------------------
" My stuff
" --------------------

" Startup calculation of makeprg for java projects
if filereadable("maven.xml")
	set makeprg=maven
elseif filereadable("build.xml")
	set makeprg=ant\ -emacs\ -logger\ org.apache.tools.ant.DefaultLogger
elseif filereadable("SConstruct")
	set makeprg=scons
endif

" Look around for a template file.
fun NewFileStuff(t)
	" First, look for a skeleton file
	let t1 = expand("~/.vim-local/skeleton." . a:t)
	if t1 != "" && filereadable(t1)
		exe ":0r " . t1
	else
		let t2 = expand("~/.vim/skeletons/skeleton." . a:t)
		if t2 != "" && filereadable(t2)
			exe ":0r " . t2
		endif
	endif
endfun

" General file stuff
fun GeneralFileStuff(t)
	" Look for an rc file
	let t1 = expand("~/.vim/scripts/" . a:t . "/rc.vim")
	if t1 != "" && filereadable(t1)
		exe ":so " . t1
	endif
endfun

" ------------------------------
" Neat tricks:
" ------------------------------

" Find ctags
if filereadable("/usr/local/bin/ctags")
	let Tlist_Ctags_Cmd = '/usr/local/bin/ctags'
else
	let Tlist_Ctags_Cmd = 'ctags'
endif
so ~/.vim/scripts/taglist.vim
