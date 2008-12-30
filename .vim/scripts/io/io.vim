" Vim syntax file
" Language: Io
" Author:   Dustin Sallings <dustin@spy.net>
" $Id: io.vim,v 1.5 2003/08/28 08:14:08 dustin Exp $

if !exists("main_syntax")
	if version < 600
		syntax clear
	elseif exists("b:current_syntax")
		finish
	endif
	let main_syntax="io"
endif

" Comments
syn keyword ioTodo contained TODO FIXME XXX

syn match ioLineComment1 "#.*" contains=ioTodo
syn match ioLineComment2 "//.*"  contains=ioTodo
syn region ioBlockComment start="/\*" end="\*/" contains=ioTodo

" Strings
syn match ioStringEscaped +\\[abfnrtv'"\\]+ contained

syn keyword ioOperator clone self super
syn keyword ioRepeat while for foreach
syn keyword ioConditional if
syn keyword ioStatement break continue yield block

syn keyword ioFunctions method
syn keyword ioFunctions read write print isOpen host empty find append
syn keyword ioFunctions catchException raiseException

syn keyword ioStorageClass Object Block Buffer CFunction Date Duration File
syn keyword ioStorageClass Future List LinkedList Map Nop Message Nil Number
syn keyword ioStorageClass String WeakLink Blowfish CGI Curses Directory
syn keyword ioStorageClass DOServer DOConnection Fnmatch Point MD5
syn keyword ioStorageClass Regex SGMLTag SQLite DBM Soup
syn keyword ioStorageClass Tree User Font Image Movie
syn keyword ioStorageClass DNSResolver Host Server Socket SocketManager
syn keyword ioStorageClass UDPReceiver UDPSender URL

syn region ioString start=+"+ end=+"+ end=+$+ contains=ioStringEscaped

if version < 508
	command -nargs=+ HiLink hi link <args>
else
	command -nargs=+ HiLink hi def link <args>
endif

HiLink ioLineComment1 Comment
HiLink ioLineComment2 Comment
HiLink ioBlockComment Comment
HiLink ioString String
HiLink ioConditional Conditional
HiLink ioRepeat Repeat
HiLink ioOperator Operator
HiLink ioStorageClass StorageClass
HiLink ioStatement Statement
HiLink ioFunctions Function
HiLink ioStringEscaped Special
delcommand HiLink

let b:current_syntax = "io"
