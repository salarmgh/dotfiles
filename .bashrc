# /etc/skel/.bashrc
#
# This file is sourced by all *interactive* bash shells on startup,
# including some apparently interactive shells such as scp and rcp
# that can't tolerate any output.  So make sure this doesn't display
# anything or bad things will happen !


# Test for an interactive shell.  There is no need to set anything
# past this point for scp and rcp, and it's important to refrain from
# outputting anything in those cases.
if [[ $- != *i* ]] ; then
	# Shell is non-interactive.  Be done now!
	return
fi

export GOPATH=/usr/local/go
export PATH=~/bin:~/.local/bin:$GOPATH/bin:$PATH
export PS1="\u@\h:\w\$ "
alias xclip='xclip -selection "clipboard"'
alias genupdate='sudo emerge -auvND --with-bdeps=y @world'
alias emacs='emacs -nw'

source ~/.helpers

source <(kubectl completion bash)
