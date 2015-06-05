# expr "$-" : .*i.* >/dev/null 2>&1 || return

shopt -s checkwinsize
shopt -s histappend
shopt -s cdable_vars

HISTCONTROL=ignoredups:ignorespace
PS1='\h:\w \$ '
export PROMPT_COMMAND='history -a'
export FIGNORE=".svn:.o:~:.class:#"
HISTSIZE=10000
HISTFILESIZE=20000

# rust
source $HOME/.cargo/env
export RUST_SRC_PATH="$(rustc --print sysroot)/lib/rustlib/src/rust/library"

# go
export GOPATH=$HOME/gocode
export PATH=$PATH:$HOME/go/bin:$GOPATH/bin:$HOME/local/bin:$HOME/.fzf/bin

alias ls='ls -BCF'
alias mv='mv -i'
alias rm='rm -i'
alias r='fc -s'
alias u='history -n'
alias s='tmux new-session -A -s emacs'

[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi
