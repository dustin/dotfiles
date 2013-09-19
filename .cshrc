# Copyright (c) 2002 Dustin Sallings

# Need this for the path stuff below
set systype=`echo $version | sed -e 's/(//g' -e 's/)//g' | awk '{print $5}'`

# This large chunk of crap is just here to set up my path
set originalpath=($path)
set path=()
# Stuff we want put in front of the path
foreach d (~/local.bin ~/bin ~/bin/$systype /opt/local/bin)
	if (-d $d) then
		set path=($path $d)
	endif
end
# OK, fix the path so far
set path=($path $originalpath)
# Don't need the original path anymore
unset originalpath
# Build out the rest of the path
foreach d (/usr/pkg/bin /usr/local/bin /usr/pkg/sbin \
		/usr/X11R6/bin /sbin /usr/sbin /usr/pkg/java/bin \
		/usr/local/teTeX/bin/powerpc-apple-darwin-current \
		/afs/@cell/system/@sys/usr/afsws/bin \
		/afs/@cell/system/@sys/usr/afs/bin )

	if (-d $d) then
		set path=($path $d)
	endif
end

# Stuff that only applies when I login
if ($?prompt) then
	set savehist=(500 merge)
	set correct=all
	set rmstar
	set noclobber
	set fignore=( \~ .bak .o )
	set autocompletion
	set nobeep
	set tperiod=10

	# set dunique
	# set dextract
	# set savedirs

	set cdpath=( $HOME )
	set watch=(all all)
	umask 022
	setenv PAGER more

	# Default editor is vi
	setenv EDITOR vi
	setenv VEEEYE vi

	# Find emacs
    if ( -f /Applications/Emacs.app/Contents/MacOS/bin/emacsclient ) then
        setenv EMACSCLIENT /Applications/Emacs.app/Contents/MacOS/bin/emacsclient
    else if ( -f /usr/local/bin/emacsclient ) then
	    setenv EMACSCLIENT /usr/local/bin/emacsclient
	else
	    setenv EMACSCLIENT emacsclient
	endif

	setenv ALTERNATE_EDITOR $VEEEYE
	setenv EDITOR $EMACSCLIENT
	setenv VISUAL $EDITOR

	set filec
	set history=1000
	set prompt = "%m:%~ \!%# "

	bindkey -k up history-search-backward
	bindkey -k down history-search-forward

	# Aliases are pulled in separately
    if ( -r ~/.aliases ) then
        source ~/.aliases
    endif

	# So are completions
	if ( -r ~/.completions ) then
		source ~/.completions
	endif

	if ( -r ~/.completions.local ) then
		source ~/.completions.local
	endif

	# Site local overrides
	if ( -r ~/.cshrc.local ) then
		source  ~/.cshrc.local
	endif

	set time="%Uu %Ss %E %P %X+%Dk %I+%Oio %Fpf+%Ww"
endif
# set autologout=30

setenv PAGER less

# Cores suck in OS X.  I'll enable it when I need it.
limit coredumpsize 0
