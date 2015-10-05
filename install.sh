#!/bin/bash -u

cd ~/
if [ -d ~/.local/dotfiles ]; then rm -fr .local/dotfiles; fi
mkdir -p .local/dotfiles
git clone https://github.com/nabinno/dotfiles.git .local/dotfiles
find ~/.local/dotfiles -maxdepth 1 -mindepth 1 -not \( \
     -name .git \
     -o -name Dockerfile \
     -o -name README.md \
     -o -name install.sh \
     \) | xargs -i cp -fr {} ~/

printf "${GREEN}"
echo '                                     '
echo '       __      __  _____ __          '
echo '  ____/ /___  / /_/ __(_) /__  _____ '
echo ' / __  / __ \/ __/ /_/ / / _ \/ ___/ '
echo '/ /_/ / /_/ / /_/ __/ / / __ (__)    '
echo '\__,_/\____/\__/_/ /_/_/\___/____/   '
echo '                                     ....is now installed!'
