# tassos zsh config - 5.8

# getting normal key functionality
bindkey "^[[3~" delete-char
bindkey "^[[3;5~" delete-word

# control
autoload -U select-word-style
select-word-style bash

# colors
autoload -U colors && colors
PS1="%(?.%F{green}%#.%F{red}!)%f "

# autocomplete
autoload -U compinit
zstyle ':completion:*' menu select
zmodload zsh/complist
compinit
# for hidden files:
_comp_options+=(globdots)

# aliases
[ -f "${XDG_CONFIG_HOME:-$HOME/.config}/aliasrc" ] && source "${XDG_CONFIG_HOME:-$HOME/.config}/aliasrc"

# move the command line to the next line if the pwd is too long.
precmd() {
    print -P "%~ "
}

# syntax highlighting
source "${HOME}/Software/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh"

# history
export HISTFILE="${XDG_CACHE_HOME:-$HOME/.cache}/zsh/zsh_history"
