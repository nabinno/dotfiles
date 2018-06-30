export REQUIRED_RUBY_VERSION=2.4.0
export REQUIRED_RUBY_VERSION_2=2.3.5
export REQUIRED_RUBY_VERSION_3=2.2.8

# ### version control ###
get-rbenv() {
  case "${OSTYPE}" in
    freebsd* | darwin* | linux*) anyenv install rbenv && exec -l zsh ;;
  esac
}

upgrade-ruby-build() {
  case "${OSTYPE}" in
    freebsd* | darwin* | linux*)
      rm -fr "$(rbenv root)"/plugins/ruby-build
      git clone https://github.com/rbenv/ruby-build.git "$(rbenv root)"/plugins/ruby-build
      ;;
  esac
}

# ----------------------------------------------------------------------
# ### installation ###
get-ruby() {
  case "${OSTYPE}" in
    cygwin) apt-cyg install ruby ;;
    freebsd* | darwin* | linux*)
      asfd install ruby $REQUIRED_RUBY_VERSION
      asfd install ruby $REQUIRED_RUBY_VERSION_2
      asfd install ruby $REQUIRED_RUBY_VERSION_3
      asdf global ruby $REQUIRED_RUBY_VERSION
      get-global-gem-packages
      ;;
  esac
}

get-ruby-by-rbenv() {
  case "${OSTYPE}" in
    cygwin) apt-cyg install ruby ;;
    freebsd* | darwin* | linux*)
      rbenv install $REQUIRED_RUBY_VERSION
      rbenv install $REQUIRED_RUBY_VERSION_2
      rbenv install $REQUIRED_RUBY_VERSION_3
      rbenv rehash
      rbenv global $REQUIRED_RUBY_VERSION
      get-global-gem-packages
      ;;
  esac
}

get-global-gem-packages() {
  gem install \
    benchmark-ips \
    bundler \
    compass \
    git-trend \
    haml \
    html2slim \
    peek-rblineprof \
    rack-lineprof \
    rails \
    rblineprof \
    rubygems-bundler \
    sidekiq \
    slim \
    stackprof \
    unicorn
  gem install rufo -v 0.1.0
}
if ! type -p ruby >/dev/null; then
  get-ruby
else
  # rm -f ~/.ruby-version
  # rbenv global $REQUIRED_RUBY_VERSION
  # _REQUIRED_RUBY_VERSION=$(echo $REQUIRED_RUBY_VERSION | sed 's/\(.*\..*\)\..*/\1/')
  # _CURRENT_RUBY_VERSION=$(ruby -v | cut -f 2 -d " " | sed 's/^\([0-9]\{1,\}\.[0-9]\{1,\}\)\..*/\1/')
  # if [[ $_REQUIRED_RUBY_VERSION > $_CURRENT_RUBY_VERSION ]]; then get-ruby; fi
fi
