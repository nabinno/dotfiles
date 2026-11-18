case "$OSTYPE" in
  darwin*)
    if [[ -x /opt/homebrew/bin/brew ]]; then
      eval "$(/opt/homebrew/bin/brew shellenv zsh)"
    fi
    ;;
  linux*)
    if [[ ! -d /run/screen ]]; then sudo mkdir -p /run/screen; fi
    if [ ! "$(stat --format '%a' /run/screen)" -eq 777 ]; then sudo chmod 777 /run/screen; fi
    ;;
esac
