#!/bin/bash -u

(
  cd ~/ || exit
  if [ -d ~/.local/dotfiles ]; then rm -fr .local/dotfiles; fi
  mkdir -p .local/dotfiles
  git clone https://github.com/nabinno/dotfiles.git .local/dotfiles
  cp -f ~/.local/dotfiles/* ~/
  cp -f ~/.local/dotfiles/.* ~/
)

printf "${GREEN}"
echo '                                     '
echo '       __      __  _____ __          '
echo '  ____/ /___  / /_/ __(_) /__  _____ '
echo ' / __  / __ \/ __/ /_/ / / _ \/ ___/ '
echo '/ /_/ / /_/ / /_/ __/ / / __ (__)    '
echo '\__,_/\____/\__/_/ /_/_/\___/____/   '
echo '                                     ....is now installed!'
