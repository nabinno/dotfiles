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
**WSL Networking:**

On WSL, `install.sh` applies `.win32/.wslconfig` to the Windows user profile
(`%USERPROFILE%\.wslconfig`). It sets `[wsl2] networkingMode=mirrored`, preserving
other settings and backing up an existing file before changing it. This requires
Windows 11 22H2 or later with a supported WSL version and allows WSL to reach
Windows services, including Chrome CDP, through `127.0.0.1`.

To apply just this configuration from an existing checkout:

```sh
bash .win32/install-wsl.sh
```

Save your work, run `wsl --shutdown` in Windows PowerShell, and reopen WSL to
activate the change. The installer does not stop WSL automatically.

**Connect to your running Windows Chrome:**

Requires Python 3 and `agent-browser`. In your normal Chrome (144+), open
`chrome://inspect/#remote-debugging` and enable remote debugging. Run `cdp`
in WSL and approve the connection in Chrome when prompted.

The command reads the default Windows Chrome profile's `DevToolsActivePort`
and connects to that browser's WebSocket endpoint. It does not launch Chrome
or fall back to a browser on a different debugging port.

```sh
cdp          # Connect agent-browser to the running Chrome
cdp --check  # Check TCP reachability (does not verify Chrome permission)
cdp --url    # Print the discovered WebSocket URL
```

`CDP_CHROME_USER_DATA_DIR` overrides the profile directory (use a WSL path).
`CDP_HOST` overrides `127.0.0.1` for an existing forwarding setup.
`CDP_URL` explicitly overrides discovery with a full `ws://` or `wss://` URL.

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
