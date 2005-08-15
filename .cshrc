# Copyright (c) 2002 Dustin Sallings
# $Id: .cshrc,v 1.14 2003/09/15 15:47:31 dustin Exp $

# Need this for the path stuff below
set systype=`echo $version | sed -e 's/(//g' -e 's/)//g' | awk '{print $5}'`

# This large chunk of crap is just here to set up my path
set originalpath=($path)
set path=()
# Stuff we want put in front of the path
foreach d (~/local.bin ~/bin ~/bin/$systype)
	if (-d $d) then
		set path=($path $d)
	endif
end
# OK, fix the path so far
set path=($path $originalpath)
# Don't need the original path anymore
unset originalpath
# Build out the rest of the path
foreach d (/usr/pkg/bin /usr/local/bin /usr/pkg/sbin /usr/X11R6/bin /sbin \
		/usr/sbin /usr/pkg/java/bin \
		/usr/local/teTeX/bin/powerpc-apple-darwin-current \
		/afs/@cell/system/@sys/usr/afsws/bin \
		/afs/@cell/system/@sys/usr/afs/bin )

	if (-d $d) then
		set path=($path $d)
	endif
end

# Bring in the fink
if ( -r /sw/bin/init.csh ) then
	source /sw/bin/init.csh
endif

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
	umask 077
	setenv PAGER more

	# Default editor is vi
	setenv EDITOR vi
	# Find vim
	if (-d $HOME/MyApps/Vim.app) then
		setenv EDITOR $HOME/MyApps/Vim.app/Contents/MacOS/Vim
	else
		which vim >& /dev/null
		if ( $? == 0 ) then
			setenv EDITOR vim
		endif
	endif

	setenv VISUAL $EDITOR
	setenv NNTPSERVER news
	setenv CVS_RSH ssh

	# Tell perforce to use my merge command
	setenv P4MERGE p4merge

	setenv IRCNICK CiscoKid
	setenv IRCNAME "Dustin Sallings"
	setenv EFIRCSERVER \
		"efnet.ipv6.xs4all.nl irc.homelien.no irc.efnet.nl \
		irc-efnet.svc.us.xo.net irc.easynews.com irc.prison.net"
	setenv FNIRCSERVER \
		"irc.ipv6.freenode.net irc.us.freenode.net irc.freenode.net"

	setenv DN uid=dustin,ou=agents,dc=spy,dc=net

	set filec
	set history=1000
	set prompt = "%m:%~ \!%# "
	set time="%Uu %Ss %E %P %X+%Dk %I+%Oio %Fpf+%Ww"

	bindkey -k up history-search-backward
	bindkey -k down history-search-forward

	setenv RUNTIMEPATH \
		/System/Library/Frameworks/JavaVM.framework/Classes/classes.jar

	setenv SmartEiffel $HOME/lib/SmartEiffel/sys/system.se

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

endif
# set autologout=30

# Cores suck in OS X.  I'll enable it when I need it.
limit coredumpsize 0
