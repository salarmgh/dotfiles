#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
PS1='[\u@\h \W]\$ '
alias xclip='xclip -selection "clipboard"'
export PATH=~/.local/bin:~/Workspace/go/bin:$PATH
export GOROOT=~/Workspace/go
