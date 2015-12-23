#!/bin/bash -u

cd ~/
if [ -d ~/.local/dotfiles ]; then rm -fr .local/dotfiles; fi
mkdir -p .local/dotfiles
git clone https://github.com/nabinno/dotfiles.git .local/dotfiles
mv -f ~/.local/dotfiles/* ~/
mv -f ~/.local/dotfiles/.* ~/
rm -fr \
   ~/.git \
   ~/Dockerfile \
   ~/README.md \
   ~/install.sh

printf "${GREEN}"
echo '                                     '
echo '       __      __  _____ __          '
echo '  ____/ /___  / /_/ __(_) /__  _____ '
echo ' / __  / __ \/ __/ /_/ / / _ \/ ___/ '
echo '/ /_/ / /_/ / /_/ __/ / / __ (__)    '
echo '\__,_/\____/\__/_/ /_/_/\___/____/   '
echo '                                     ....is now installed!'
