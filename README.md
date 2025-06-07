# nabinno's dotfiles

Personal dotfiles repository for Unix-like systems with modular configurations for Zsh and Emacs development environments.

<img src=dotfiles.jpg width=400 /> <sup id="a1">[[1]](#f1)</sup>

## Features

- **Modular Emacs Configuration**: 100+ feature-specific modules using modern `leaf` package manager
- **Intelligent Zsh Functions**: Autoloaded functions with Go utilities for complex shell operations
- **Cross-Platform Support**: OS detection and platform-specific initialization
- **Development Tools Integration**: LSP, Git workflows, language-specific configurations
- **Claude Code Integration**: AI-assisted development with Emacs integration

## Quick Start

### Prerequisites
1. Install Zsh
2. [Connect to GitHub with SSH](https://docs.github.com/authentication/connecting-to-github-with-ssh/generating-a-new-ssh-key-and-adding-it-to-the-ssh-agent?platform=linux)

### Installation
```sh
sh -c "$(curl -fsSL https://raw.github.com/nabinno/dotfiles/master/install.sh)"
```

### Setup
**Zsh Environment:**
```sh
exec zsh -l
```

**Emacs Environment:**
```sh
emacs --debug-init -nw
```

## Architecture

### Configuration Structure
- **Root Level**: Core dotfiles (`.zshrc`, `.gitconfig`, `.screenrc`, etc.)
- **`.emacs.d/`**: Modular Emacs configuration with init system
- **`.zsh.d/function/`**: Zsh autoload functions with Go-based utilities

### Key Components
- **Emacs**: Uses `leaf` package manager with 100+ feature modules
- **Zsh Functions**: Autoloaded via `FPATH` with platform detection
- **Go Utilities**: Complex shell operations in `.zsh.d/function/`
- **Installation**: Clones to `~/.local/dotfiles` and copies files to `$HOME`

### Environment Variables
- `DOTFILES_PATH`: Automatically set (via `ghq` or `~/.local/dotfiles`)
- `FPATH`: Extended for custom function autoloading

## Development

### Testing Configuration
```sh
# Test Emacs configuration
emacs --debug-init

# Test Zsh functions
exec zsh -l

# Test Go utilities
cd .zsh.d/function && go run .
```

### Adding New Configurations
1. **Emacs**: Add `init-feature.el` to `.emacs.d/lisp/`
2. **Zsh**: Add function to `.zsh.d/function/` and call via `require` in `.zshrc`
3. **Go**: Extend utilities in `.zsh.d/function/` directory

---

## License
MIT

## Epilogue
>     A whale!
>     Down it goes, and more, and more
>     Up goes its tail!
>
>     -Buson Yosa

---

<b id="f1">[1]</b> "A selection of mallets, bores and files" (1700-1799, Robert Benard after Jacques-Raymond Lucotte) credited in Wellcome Collection is licensed by [CC 4.0 BY](https://creativecommons.org/licenses/by/4.0/) [↩](#a1)
