get-brew() {
  case "${OSTYPE}" in
    darwin*) ;;
    linux*)
      sh -c "$(curl -fsSL https://raw.githubusercontent.com/Linuxbrew/install/master/install.sh)"
      brew vendor-install ruby
      ;;
  esac
}

get-base-brew-packages() {
  case "${OSTYPE}" in
    darwin*)
      brew install \
        jq \
        memcached \
        the_silver_searcher \
        tree
      ;;
    linux*)
      case "${DIST}" in
        Redhat | RedHat)
          brew install homebrew/dupes/gperf
          ;;
        Debian | Ubuntu)
          brew install \
            jq
          ;;
      esac
      ;;
  esac
}

case "${OSTYPE}" in
  darwin*) ;;
  linux*)
    case $DIST in
      RedHat | Redhat | Debian)
        if [ -d /home/linuxbrew/.linuxbrew ]; then
          export MANPATH=/home/linuxbrew/.linuxbrew/share/man:$MANPATH
          export INFOPATH=/home/linuxbrew/.linuxbrew/share/info:$INFOPATH
          # export LD_LIBRARY_PATH=/home/linuxbrew/.linuxbrew/lib:$LD_LIBRARY_PATH
          export PATH="/home/linuxbrew/.linuxbrew/bin:$PATH"
        elif [ -d ~/.linuxbrew ]; then
          export MANPATH=$HOME/.linuxbrew/share/man:$MANPATH
          export INFOPATH=$HOME/.linuxbrew/share/info:$INFOPATH
          export LD_LIBRARY_PATH=$HOME/.linuxbrew/lib:$LD_LIBRARY_PATH
          export PATH="$HOME/.linuxbrew/bin:$PATH"
        fi
        ;;
      Ubuntu)
        case $DIST_VERSION in
          12.04 | 14.04)
            if [ -d /home/linuxbrew/.linuxbrew ]; then
              export MANPATH=/home/linuxbrew/.linuxbrew/share/man:$MANPATH
              export INFOPATH=/home/linuxbrew/.linuxbrew/share/info:$INFOPATH
              # export LD_LIBRARY_PATH=/home/linuxbrew/.linuxbrew/lib:$LD_LIBRARY_PATH
              export PATH="/home/linuxbrew/.linuxbrew/bin:$PATH"
            elif [ -d ~/.linuxbrew ]; then
              export MANPATH=$HOME/.linuxbrew/share/man:$MANPATH
              export INFOPATH=$HOME/.linuxbrew/share/info:$INFOPATH
              export LD_LIBRARY_PATH=$HOME/.linuxbrew/lib:$LD_LIBRARY_PATH
              export PATH="$HOME/.linuxbrew/bin:$PATH"
            fi
            ;;
          16.04)
            if [ -d /home/linuxbrew/.linuxbrew ]; then
              export MANPATH=/home/linuxbrew/.linuxbrew/share/man:$MANPATH
              export INFOPATH=/home/linuxbrew/.linuxbrew/share/info:$INFOPATH
              export LD_LIBRARY_PATH=/home/linuxbrew/.linuxbrew/lib:$LD_LIBRARY_PATH
              export PATH="/home/linuxbrew/.linuxbrew/bin:$PATH"
            elif [ -d ~/.linuxbrew ]; then
              export MANPATH=$HOME/.linuxbrew/share/man:$MANPATH
              export INFOPATH=$HOME/.linuxbrew/share/info:$INFOPATH
              export LD_LIBRARY_PATH=$HOME/.linuxbrew/lib:$LD_LIBRARY_PATH
              export PATH="$HOME/.linuxbrew/bin:$PATH"
            fi
            ;;
        esac
        ;;
    esac
    ;;
esac
if ! type -p brew >/dev/null; then get-brew && get-base-brew-packages; fi
