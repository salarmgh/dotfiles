#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
PS1='[\u@\h \W]\$ '
alias xclip='xclip -selection "clipboard"'
export PATH=~/.local/bin:~/Workspace/go/bin:~/.emacs.d/bin:~/Workspace/go/bin:$PATH
export GOPATH=~/Workspace/go
export GO111MODULE=on
export GOPRIVATE="git.digikala.com/*"
