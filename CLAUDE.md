# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is nabinno's personal dotfiles repository containing configuration files for Unix-like systems. The repository is designed to provision development environments with Zsh and Emacs configurations.

## Installation and Setup Commands

### Initial Installation
```bash
# Install dotfiles to home directory
sh -c "$(curl -fsSL https://raw.github.com/nabinno/dotfiles/master/install.sh)"
```

### Provision Zsh Environment
```bash
# Launch zsh or reload configuration
exec zsh -l
```

### Provision Emacs Environment
```bash
# Launch Emacs with debug mode
emacs --debug-init -nw
```

### Go Module Management
```bash
# In .zsh.d/function directory - check dependencies
go mod why

# Run the Go initialization utility
go run .

# Test Go utilities
cd .zsh.d/function && go test
```

## Architecture Overview

### Configuration Structure
- **Root Level**: Core dotfiles (`.zshrc`, `.gitconfig`, `.screenrc`, etc.)
- **`.emacs.d/`**: Modular Emacs configuration with init system
- **`.zsh.d/function/`**: Zsh autoload functions with Go-based utilities

### Emacs Configuration Architecture
The Emacs setup uses a modular initialization system located in `.emacs.d/lisp/`:
- `init.el`: Bootstrap file that loads all modules
- `init-*.el` files: Feature-specific configurations (100+ modules)
- Uses `leaf` package manager for dependency management
- Organized by functionality: languages, tools, UI components

Key initialization modules:
- `init-leaf.el`: Package management
- `init-exec-path.el`: Environment setup
- `init-site-lisp.el`: Local packages
- Language-specific: `init-ruby-mode.el`, `init-golang.el`, `init-python-mode.el`, etc.
- Recent additions: `init-llm.el` for Claude Code integration

### Zsh Function System
The `.zsh.d/function/` directory contains:
- Platform detection and OS-specific initialization
- Tool installation functions (`init-docker`, `init-git`, `init-ruby`, etc.)
- Go utilities for complex shell operations
- Autoloaded functions via `FPATH` configuration

### Installation Strategy
The `install.sh` script:
1. Clones repository to `~/.local/dotfiles`
2. Copies all dotfiles to home directory
3. Removes repository metadata files
4. Uses `ghq` for repository management when available

## Development Workflow

### Testing Emacs Configuration
```bash
# Test Emacs config with debug output
emacs --debug-init

# Check for syntax errors in init files
emacs --batch --eval "(byte-compile-file \"~/.emacs.d/init.el\")"

# Test specific init file
emacs --batch --eval "(byte-compile-file \"~/.emacs.d/lisp/init-llm.el\")"

# Reload Emacs configuration without restart
emacs --eval "(load-file \"~/.emacs.d/init.el\")"
```

### Working with Zsh Functions
Functions are autoloaded via the `require` helper in `.zshrc`. The Go utility in `.zsh.d/function/` handles complex initialization tasks.

### Testing Configuration Changes
```bash
# Test Zsh configuration reload
exec zsh -l

# Debug Zsh function loading
zsh -x -c "source ~/.zshrc"

# Check if specific function is loaded
which init-ruby

# Manual function reload
unfunction init-ruby && autoload init-ruby
```

### Environment Variables
- `DOTFILES_PATH`: Set automatically to repository location
- `FPATH`: Extended to include custom function directory

## Key Dependencies

### Go Modules
- `github.com/mattn/go-shellwords`: Shell command parsing in Go utilities

### Emacs Packages
- `leaf`: Modern package configuration macro
- Language support: LSP, various major modes  
- Development tools: projectile, magit, flycheck
- Claude Code integration: `claude-code` package for AI assistance