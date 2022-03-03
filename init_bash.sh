export EDITOR='emacsclient' PAGER=cat TERM=xterm-256color
export GOPATH=$HOME/go
export PATH=$GOPATH/bin:$HOME/local/bin:$HOME/.fzf/bin:$PATH
PS0='\r`printf "%*s" $(($COLUMNS - 20)) ""`\t \d\n' PS1='\h:\w \$ '
