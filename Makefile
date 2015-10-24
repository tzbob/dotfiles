dotfiles:
	dotty/dotty.py -r dotty.json

shell:
	sh -c "$$(curl -fsSL https://raw.github.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"

spacemacs:
	git clone --recursive https://github.com/syl20bnr/spacemacs ~/.emacs.d
