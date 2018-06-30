# Emacs
export REQUIRED_EMACS_VERSION=25.3
export UNREQUIRED_EMACS_VERSION=24.5

# ----------------------------------------------------------------------
# ### installation ###
get-emacs() {
  case "${OSTYPE}" in
    msys) pacman -S mingw-w64-x86_64-emacs ;;
    cygwin) apt-cyg install emacs ;;
    freebsd* | darwin* | linux*)
      case "${DIST}" in
        Redhat | RedHat)
          sudo yum install -y ncurses-devel
          local current_pwd=$(pwd)
          wget https://ftp.gnu.org/gnu/emacs/emacs-$REQUIRED_EMACS_VERSION.tar.gz
          tar zxf emacs-$REQUIRED_EMACS_VERSION.tar.gz
          cd emacs-$REQUIRED_EMACS_VERSION
          ./configure --with-xpm=no --with-gif=no --with-x-toolkit=no --with-tiff=no
          make
          yes | sudo make install
          cd $current_pwd
          rm -fr emacs-$REQUIRED_EMACS_VERSION*
          ;;
        Debian | Ubuntu)
          case $DIST_VERSION in
            16.04)
              sudo add-apt-repository ppa:kelleyk/emacs
              sudo apt update
              sudo apt install emacs25
              ;;
            *)
              sudo apt-get install build-essential checkinstall
              sudo apt-get build-dep
              # sudo apt-get build-dep emacs24
              sudo apt-get install lincurses5-dev
              wget https://ftp.gnu.org/gnu/emacs/emacs-$REQUIRED_EMACS_VERSION.tar.gz
              tar zxf emacs-$REQUIRED_EMACS_VERSION.tar.gz
              cd emacs-$REQUIRED_EMACS_VERSION
              ./configure --with-xpm=no --with-gif=no --with-x-toolkit=no --with-tiff=no --with-jpeg=no --with-png=no
              make
              yes | sudo make install
              cd $current_pwd
              rm -fr emacs-$REQUIRED_EMACS_VERSION*
              ;;
          esac
          # sudo add-apt-repository ppa:ubuntu-elisp/ppa
          # sudo apt-get update
          # sudo apt-get install emacs-snapshot ;;
          ;;
      esac
      ;;
  esac
}
case $OSTYPE in msys) alias emacs='/mingw64/bin/emacs -nw' ;; esac
if ! type emacs >/dev/null; then
  get-emacs
else
  _CURRENT_EMACS_VERSION=$(emacs --version | head -n 1 | sed 's/GNU Emacs //' | awk '$0 = substr($0, 1, index($0, ".") + 1)')
  if [[ $_REQUIRED_EMACS_VERSION > $_CURRENT_EMACS_VERSION ]]; then get-emacs; fi
fi

# ----------------------------------------------------------------------
# ### uninstall ###
remove-emacs() {
  case $OSTYPE in
    linux*)
      case $DIST in
        Ubuntu)
          sudo apt remove emacs24
          wget https://ftp.gnu.org/gnu/emacs/emacs-$UNREQUIRED_EMACS_VERSION.tar.gz
          tar zxf emacs-$UNREQUIRED_EMACS_VERSION.tar.gz
          cd emacs-$UNREQUIRED_EMACS_VERSION
          ./configure --with-xpm=no --with-gif=no --with-x-toolkit=no --with-tiff=no --with-jpeg=no --with-png=no
          make
          yes | sudo make uninstall
          cd $current_pwd
          rm -fr emacs-$UNREQUIRED_EMACS_VERSION*
          ;;
      esac
      ;;
  esac
}

# ----------------------------------------------------------------------
# ### aspell ###
get-aspell() {
  case "${OSTYPE}" in
    darwin*) brew install --with-lang-ena aspell ;;
    freebsd*) ;;
    linux*)
      case "${DIST}" in
        Debian | Ubuntu) sudo apt-get install -y aspell ;;
      esac
      ;;
  esac
}
if ! type -p aspell >/dev/null; then get-aspell; fi

# ----------------------------------------------------------------------
# ### mu4e ###
get-mu() {
  case "${OSTYPE}" in
    freebsd* | darwin* | linux*)
      case "${DIST}" in
        Debian) ;;
        Ubuntu)
          case $DIST_VERSION in
            14.04)
              sudo apt-get install -y \
                libgmime-2.6-dev \
                libxapian-dev \
                gnutls-bin \
                guile-2.0-dev \
                html2text \
                xdg-utils \
                offlineimap
              \git clone https://github.com/djcb/mu
              cd mu
              sudo autoreconf -i
              ./configure && make
              sudo make install
              cd .. && sudo rm -fr mu
              ln -s /usr/local/share/emacs/site-lisp/mu4e $HOME/.emacs.d/site-lisp/
              ;;
          esac
          ;;
      esac
      ;;
  esac
}

mu-restart() {
  mv ~/Maildir ~/Maildir.org$(date +%y%m%d)
  mkdir -p ~/Maildir
  rm -fr ~/.mu
  rm -fr ~/.offlineimap
  offlineimap
  mu index --maildir=~/Maildir
  mu index --rebuild
  mu index
}
# if ! type -p mu > /dev/null ; then get-mu ; fi

# ----------------------------------------------------------------------
# ### sekka ###
set-sekka() {
  case "${OSTYPE}" in
    freebsd* | linux*) docker restart sekka ;;
  esac
}

get-sekka() {
  case "${OSTYPE}" in
    freebsd* | linux*) docker run -p 12929:12929 -d --name=sekka -t kiyoka/sekka ;;
  esac
}

# ----------------------------------------------------------------------
# Ctags
export REQUIRED_CTAGS_VERSION=5.8

get-ctags() {
  case "${OSTYPE}" in
    freebsd*linux*)
      local current_pwd=$(pwd)
      wget http://prdownloads.sourceforge.net/ctags/ctags-$REQUIRED_CTAGS_VERSION.tar.gz
      tar zxf ctags-$REQUIRED_CTAGS_VERSION.tar.gz
      cd ctags-$REQUIRED_CTAGS_VERSION
      ./configure --prefix=$HOME/.local
      make
      sudo make install
      cd $current_pwd
      ;;
  esac
}
if ! type -p ctags >/dev/null; then get-ctags; fi

alias ctags=~/.local/bin/ctags

# ----------------------------------------------------------------------
# Pandoc
get-pandoc() {
  case $OSTYPE in
    freebsd* | darwin*) nix-install pandoc ;;
    linux*)
      case $DIST_VERSION in
        14.04 | 16.04) ;;
        *) nix-install pandoc ;;
      esac
      ;;
  esac
}
if ! type -p pandoc >/dev/null; then get-pandoc; fi

alias pandocpdf="pandoc -V documentclass=ltjarticle --latex-engine=lualatex -t pdf"
alias pandocslide="pandoc -t slidy -s"
