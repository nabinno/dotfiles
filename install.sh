#!/bin/bash -u

cd ~/
if [ ! -d ~/.local ]; then mkdir -p .local; fi
git clone https://github.com/nabinno/dotfiles.git .local/
find ~/.local/dotfiles -maxdepth 1 -mindepth 1 | xargs -i cp -fR {} ~/
rm -fr .git Dockerfile README.md
``
printf "${GREEN}"
echo '       __ '
echo '  ____/ / '
echo ' / __ _/  '
echo '/ /_/ /   '
echo '\____/    '
echo '           ....is now installed!'
