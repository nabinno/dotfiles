#!/usr/bin/env zsh

if type -p ghq >/dev/null; then
  export DOTFILES_PATH=$(ghq root)/github.com/nabinno/dotfiles
  if [ ! -d $DOTFILES_PATH ]; then
    ghq get nabinno/dotfiles
    wait
  fi
else
  export DOTFILES_PATH=~/.local/dotfiles
  if [ ! -d $DOTFILES_PATH ]; then
    mkdir -p ~/.local
    sh -c "$(curl -fsSL https://raw.github.com/nabinno/dotfiles/master/install)"
    wait
  fi
fi

export FPATH=$DOTFILES_PATH/.zsh.d/function:$FPATH

require() {
  autoload $1
  $1
}

require_other() {
  (
    cd $DOTFILES_PATH/.zsh.d/function
    GO111MODULE=off go run .
  )
}

# Base
require init-os-detect
(require init-os-update 2 &)
require init-environmental-variable
(require init-base-installation 2 &)
(require init-shell 2 &)
(require init-windows-management-framework 2 &)
(require init-chocolatey 2 &)
# require init-nix
# require init-chef
require init-anyenv
require init-asdf
(require init-docker 2 &)
require init-docker--util
require init-homebrew
# (require init-autoparts 2 &)

# ProgrammingLanguage
require init-go
(require_other 2 &)
# (require init-ruby 2 &) # migrating
require init-ruby--util
require init-elixir
(require init-crystal 2 &)
(require init-ocaml 2 &)
(require init-haskell 2 &)
# (require init-dotnet 2&)
require init-java
(require init-php 2 &)
require init-php--util
(require init-python 2 &)
(require init-perl 2 &)
(require init-javascript 2 &)
require init-rust
(require init-red 2 &)
# (require init-remote-procedure-call 2 &)

# Daemon
# (require init-postgresql 2 &)
# (require init-mysql 2 &)
# (require init-redis 2 &)
# (require init-memcached 2 &)
# (require init-nginx 2 &)
require init-dns

# IntegratedDevelopmentEnvironment
(require init-emacs 2 &)
(require init-file-system 2 &)
(require init-git 2 &)
require init-git--util
# (require init-plantuml 2 &)
# (require init-imagemagick 2 &)
(require init-benchmark 2 &)
(require init-vagrant 2 &)
(require init-wercker 2 &)
(require init-ansible 2 &)
(require init-terraform 2 &)
require init-terraform--util
require init-zsh
(require init-powershell 2 &)
require init-screen
# (require init-asciinema 2 &)
# (require init-slack 2 &)
# require init-hatena-bookmark

# Platform
(require init-heroku 2 &)
require init-google-cloud-platform
# (require init-kubernetes 2 &)
(require init-amazon-web-services 2 &)

# Other
require init-customize
if [ -f ~/.zsh.d/init.zsh ]; then source ~/.zsh.d/init.zsh; fi
