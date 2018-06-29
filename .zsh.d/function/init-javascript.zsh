export REQUIRED_NODE_VERSION='6.9.1'
export node='NODE_NO_READLINE=1 node'
case $OSTYPE in msys | cygwin) export PATH=$(get-winpath "C:\Program Files\nodejs"):$PATH ;; esac

# ----------------------------------------------------------------------
# ### version control ###
get-ndenv() {
  case "${OSTYPE}" in
    freebsd* | darwin* | linux*)
      anyenv install ndenv
      git clone https://github.com/pine/ndenv-yarn-install.git "$(ndenv root)/plugins/ndenv-yarn-install"
      exec -l zsh
      ;;
  esac
}

# ----------------------------------------------------------------------
# ### installation ###
get-node() {
  case "$OSTYPE" in
    msys | cygwin) choco install nodejs ;;
    linux*)
      rm -rf ~/.asdf/keyrings/nodejs
      ~/.asdf/plugins/nodejs/bin/import-release-team-keyring
      asdf install nodejs $REQUIRED_NODE_VERSION
      asdf global nodejs $REQUIRED_NODE_VERSION
      ;;
  esac
}

get-node-with-ndenv() {
  case "$OSTYPE" in
    msys | cygwin) choco install nodejs ;;
    linux*)
      ndenv install v$REQUIRED_NODE_VERSION
      ndenv rehash
      ndenv global v$REQUIRED_NODE_VERSION
      ;;
    # get-global-npm-packages ;;
  esac
}

set-node-with-ndenv() {
  case "$OSTYPE" in
    linux*) ndenv global v$REQUIRED_NODE_VERSION ;;
  esac
}

# ----------------------------------------------------------------------
# Npm
get-global-npm-packages() {
  npm install -g \
    bower \
    grunt-cli \
    gulp \
    hexo-cli \
    html2jade \
    http-server \
    less \
    node-plantuml \
    npm2dot \
    phantomjs \
    requirejs \
    yamljs \
    tern
}
if ! type -p npm >/dev/null; then get-node; fi

rebuild-sass() {
  npm uninstall --save-dev gulp-sass
  npm install --save-dev gulp-sass@2
  npm rebuild node-sass
}

get-watchman() {
  case $OSTYPE in
    linux*)
      git clone https://github.com/facebook/watchman.git
      cd watchman
      sh ./autogen.sh
      ./configure
      make
      sudo make install
      cd ..
      sudo rm -fr watchman
      ;;
  esac
}
# if ! type -p watchman > /dev/null; then get-watchman; fi

get-yoeman() {
  npm i -g \
    generator-standard-readme \
    yo
}
