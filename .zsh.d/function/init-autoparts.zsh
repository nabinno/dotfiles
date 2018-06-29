case "${OSTYPE}" in
  linux*) case "${DIST}" in
    Debian | Ubuntu)
      export PATH="$HOME/.parts/autoparts/bin:$PATH"
      export PATH="$HOME/.parts/lib/node_modules/less/bin:$PATH"

      get-parts() {
        get-base
        ruby -e "$(curl -fsSL https://raw.github.com/nitrous-io/autoparts/master/setup.rb)"
        eval "$(parts env)"
        get-parts-packages
      }

      get-parts-packages() {
        parts install \
          heroku_toolbelt \
          phantomjs \
          the_silver_searcher \
          tree \
          uuid
      }
      if ! type -p parts >/dev/null; then get-parts; fi
      if type -p parts >/dev/null; then eval "$(parts env)"; fi
      ;;
  esac ;;
esac
