#!/bin/bash -u

cd ~/
mkdir -p .local
git clone https://github.com/nabinno/dotfiles.git .local/
find ~/dotfiles -maxdepth 1 -mindepth 1 | xargs -i cp -fR {} ~/
rm -fr .git Dockerfile README.md
``
printf "${GREEN}"
echo '       __ '
echo '  ____/ / '
echo ' / __ _/  '
echo '/ /_/ /   '
echo '\____/    '
echo '           ....is now installed!'
