# dotfiles --- devenv provisioner
## Provision scripts
- Zsh
- Emacs lisp

## Installation

```
git clone https://github.com/nabinno/dotfiles.git
find ~/dotfiles -maxdepth 1 -mindepth 1 | xargs -i mv -f {} ~/
rm -fr .git Dockerfile README.md
```

## Provision
### Zsh
Launch zsh or run zsh resource.
```
zsh -c "source ~/.zshrc"
```

### Emacs
Launch Emacs.
```
emacs
```

---

## EPILOGUE
>     A whale!
>     Down it goes, and more, and more
>     Up goes its tail!
>
>     -Buson Yosa
