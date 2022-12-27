#-*-sh-*-
if systemctl -q is-active graphical.target && [[ ! $DISPLAY && $XDG_VTNR -eq 1 ]]; then
  exec startx
fi

export PATH="$PATH:/home/bob/.cache/scalacli/local-repo/bin/scala-cli"
eval "$(/home/bob/.linuxbrew/bin/brew shellenv)"
