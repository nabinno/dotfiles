#!/usr/bin/env zsh

# == INDEX ==
# Base
# ProgrammingLanguage
# Daemon
# IntegratedDevelopmentEnvironment
# Platform
# Other

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
    go mod why
    go run .
  )
}

# Base
require init-os-detect
require init-os-update
require init-environmental-variable
require init-base-installation
require init-windows-management-framework
require init-chocolatey
require init-nix
# require init-chef
require init-docker
require init-docker--util
require init-homebrew
require init-anyenv
require init-asdf
# require init-autoparts

# ProgrammingLanguage
require init-go
require_other
require init-shell
require init-ruby
require init-ruby--util
require init-elixir
require init-crystal
require init-ocaml
require init-haskell
# require init-dotnet
require init-java
require init-php
require init-php--util
require init-python
require init-perl
require init-javascript
require init-rust
require init-red
# require init-remote-procedure-call

# Storage
require init-csv
# require init-postgresql
# require init-mysql
# require init-redis
# require init-memcached

# Daemon
# require init-nginx
require init-dns

# IntegratedDevelopmentEnvironment
require init-emacs
require init-file-system
require init-git
require init-git--util
# require init-plantuml
# require init-imagemagick
require init-benchmark
require init-security
# require init-vagrant
# require init-wercker
require init-ansible
require init-terraform
require init-packer
require init-zsh
require init-powershell
require init-screen
require init-llm
# require init-asciinema
# require init-slack
# require init-hatena-bookmark

# Platform
# require init-heroku
require init-google-cloud-platform
# require init-kubernetes
require init-amazon-web-services

# Other
require init-customize
if [ -f ~/.zsh.d/init.zsh ]; then source ~/.zsh.d/init.zsh; fi
