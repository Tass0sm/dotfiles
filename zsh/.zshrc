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

# dir tricks
autoload -Uz chpwd_recent_dirs cdr add-zsh-hook
add-zsh-hook chpwd chpwd_recent_dirs

# Conda

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/home/tassos/Software/miniconda2-tmp/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/home/tassos/Software/miniconda2-tmp/etc/profile.d/conda.sh" ]; then
        . "/home/tassos/Software/miniconda2-tmp/etc/profile.d/conda.sh"
    else
        export PATH="/home/tassos/Software/miniconda2-tmp/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<
