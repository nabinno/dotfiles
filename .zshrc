#!/usr/bin/env zsh

export FPATH=~/.zsh.d/function:$FPATH

require() {
  autoload $1
  $1
}

require_other() {
  (
    cd /home/app/.zsh.d/function
    go run .
  )
}

# Env
export REQUIRED_RUBY_VERSION=2.5.3
export GEMSRC_USE_GHQ=~/.ghq.d/src

# Base
require init-os-detect
require init-os-update
require init-environmental-variable
require init-base-installation
require init-shell
require init-windows-management-framework
require init-chocolatey
# require init-nix
# require init-chef
require init-anyenv
require init-asdf
require init-docker
require init-homebrew
# require init-autoparts

# ProgrammingLanguage
require init-go
require_other
require init-ruby # migrating
require init-elixir
require init-crystal
require init-ocaml
require init-haskell
# require init-dotnet
require init-java
require init-php
require init-python
require init-perl
require init-javascript
require init-rust
require init-red
# require init-remote-procedure-call

# Daemon
# require init-postgresql
# require init-mysql
# require init-redis
# require init-memcached
# require init-nginx
require init-dns

# IntegratedDevelopmentEnvironment
require init-emacs
require init-file-system
require init-git
# require init-plantuml
# require init-imagemagick
require init-benchmark
require init-vagrant
require init-wercker
require init-ansible
require init-terraform
require init-zsh
require init-powershell
require init-screen
# require init-asciinema
require init-slack
require init-hatena-bookmark

# Platform
require init-heroku
require init-google-cloud-platform
# require init-kubernetes
require init-amazon-web-services

# Other
require init-customize
if [ -f ~/.zsh.d/init.zsh ]; then source ~/.zsh.d/init.zsh; fi
