# expr "$-" : .*i.* >/dev/null 2>&1 || return
# if [[ -n $INSIDE_EMACS ]];then
#   PS1=''
# fi

shopt -s checkwinsize
shopt -s histappend
shopt -s cdable_vars

HISTCONTROL=ignoredups:ignorespace
export PROMPT_COMMAND='history -a'
export FIGNORE=".svn:.o:~:.class:#"
HISTSIZE=10000
HISTFILESIZE=20000

# rust
if test -f $HOME/.cargo/env; then
  source $HOME/.cargo/env
  export RUST_SRC_PATH="$(rustc --print sysroot)/lib/rustlib/src/rust/library"
fi

# go
export GOPATH=$HOME/go
export PATH=$GOPATH/bin:$HOME/local/bin:$HOME/.fzf/bin:$PATH

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

if test -f $HOME/.asdf/asdf.sh; then
    . $HOME/.asdf/asdf.sh
    expr "$-" : .*i.* >/dev/null 2>&1 && . $HOME/.asdf/completions/asdf.bash
fi

if [[ -z $EMACS_BASH_COMPLETE ]]; then
  PS1='\h:\w \$ ' PS0='`tput cuu1``tput cuf $(($(tput cols) - 15))`\t `date +%m/%d`\n'
fi

show_date_before_cmd() {
    [[ "$BASH_COMMAND" = "$PROMPT_COMMAND" ]] || printf "%*s\e[3m\e[4m%s\e[0m\n" $((COLUMNS-32)) "" "$(date)"
}

if [[ -n "$INSIDE_EMACS" ]]; then
    if expr $INSIDE_EMACS : .*comint.* >/dev/null 2>&1; then
	export EDITOR='emacsclient' PAGER=cat TERM=xterm-256color PS0=
	PROMPT_COMMAND="trap 'show_date_before_cmd; trap - DEBUG' DEBUG${PROMPT_COMMAND:+;}${PROMPT_COMMAND}"
    else
	[[ -n "$EAT_SHELL_INTEGRATION_DIR" ]] && source "$EAT_SHELL_INTEGRATION_DIR/bash"
    fi
fi
