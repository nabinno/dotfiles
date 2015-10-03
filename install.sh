#!/bin/bash -u

cd ~/
git clone https://github.com/nabinno/dotfiles.git
find ~/dotfiles -maxdepth 1 -mindepth 1 | xargs -i mv -f {} ~/
rm -fr .git Dockerfile README.md
``
printf "${GREEN}"
echo '       __ '
echo '  ____/ / '
echo ' / __ _/  '
echo '/ /_/ /   '
echo '\____/    '
echo '           ....is now installed!'
