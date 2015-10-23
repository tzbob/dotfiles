dotfiles:
	dotty/dotty.py -r dotty.json

shell:
	sh -c "$(curl -fsSL https://raw.github.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"
