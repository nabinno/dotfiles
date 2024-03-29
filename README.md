# nabinno's dotfiles
This is a repository with my configuration files, those that in *nix normally are these files under the $HOME directory.

<img src=dotfiles.jpg width=400 /> <sup id="a1">[[1]](#f1)</sup>

## Provision scripts
- Zsh
- Emacs lisp

## Installation
1. Install Zsh
2. [Connect to GitHub with SSH](https://docs.github.com/authentication/connecting-to-github-with-ssh/generating-a-new-ssh-key-and-adding-it-to-the-ssh-agent?platform=linux)
3. Download dotfiles
    ```sh
    sh -c "$(curl -fsSL https://raw.github.com/nabinno/dotfiles/master/install.sh)"
    ```

## Provision
### Zsh
Launch zsh or run zsh resource.
```sh
exec zsh -l
```

### Emacs
Launch Emacs.
```sh
emacs --debug-init -nw
```

---

## LISENCE
MIT

## EPILOGUE
>     A whale!
>     Down it goes, and more, and more
>     Up goes its tail!
>
>     -Buson Yosa

---

<b id="f1">[1]</b> "A selection of mallets, bores and files" (1700-1799, Robert Benard after Jacques-Raymond Lucotte) credited in Wellcome Collection is licensed by [CC 4.0 BY](https://creativecommons.org/licenses/by/4.0/) [↩](#a1)
