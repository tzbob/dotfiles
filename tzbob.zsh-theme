# Simple theme based on my old zsh settings.

PROMPT='%{$fg[yellow]%}>%{$reset_color%} '
RPROMPT='%{$fg[yellow]%}%~%{$reset_color%}$(git_prompt_info)'

ZSH_THEME_GIT_PROMPT_DIRTY="%{$fg[yellow]%}✗%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_PREFIX="  "
ZSH_THEME_GIT_PROMPT_SUFFIX=""