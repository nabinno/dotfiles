export REQUIRED_PERL_VERSION=5.20.3
export PATH="$HOME/.cask/bin:$PATH"

# ----------------------------------------------------------------------
# ### version control ###
get-plenv() {
  case "${OSTYPE}" in
    freebsd* | darwin* | linux*) anyenv install plenv && exec -l zsh ;;
  esac
}
if ! type -p plenv >/dev/null; then get-plenv; fi

# ----------------------------------------------------------------------
# ### installation ###
get-perl() {
  case "${OSTYPE}" in
    cygwin) apt-cyg install perl ;;
    freebsd* | darwin*)
      plenv install $REQUIRED_PERL_VERSION
      plenv rehash
      plenv global $REQUIRED_PERL_VERSION
      plenv install-cpanm
      ;;
    linux*)
      case $DIST in
        Redhat | RedHat)
          nix-install perl-$REQUIRED_PERL_VERSION
          nix-install perl-App-cpanminus
          ;;
        Debian | Ubuntu)
          plenv install $REQUIRED_PERL_VERSION
          plenv rehash
          plenv global $REQUIRED_PERL_VERSION
          plenv install-cpanm
          ;;
      esac
      ;;
  esac
}
if ! type -p perl >/dev/null; then get-perl; fi
# eval $(perl -I$HOME/.local/lib/perl5 -Mlocal::lib=$HOME/.local)

# ----------------------------------------------------------------------
# ### plagger ###
get-plagger() {
  case "${OSTYPE}" in
    cygwin) ;;
    freebsd* | darwin*)
      cpanm -fi YAML::Loader \
        XML::LibXML \
        XML::LibXML::SAX \
        XML::LibXML::XPathContext \
        XML::Liberal \
        Text::Glob \
        Module::Runtime \
        Params::Util \
        Digest::SHA1 \
        Class::Load \
        XML::RSS \
        XML::RSS::LibXML \
        XML::RSS::Liberal \
        XML::Feed \
        XML::Feed::RSS \
        XML::Atom \
        WebService::Bloglines \
        Plagger
      ;;
    linux*)
      case $DIST in
        Redhat | RedHat) ;;
        Debian | Ubuntu)
          cpanm -fi YAML::Loader \
            XML::LibXML \
            XML::LibXML::SAX \
            XML::LibXML::XPathContext \
            XML::Liberal \
            Text::Glob \
            Module::Runtime \
            Params::Util \
            Digest::SHA1 \
            Class::Load \
            XML::RSS \
            XML::RSS::LibXML \
            XML::RSS::Liberal \
            XML::Feed \
            XML::Feed::RSS \
            XML::Atom \
            WebService::Bloglines \
            Plagger
          ;;
      esac
      ;;
  esac
}

# ----------------------------------------------------------------------
# ### org-asana ###
get-org-asana() {
  yes | cpanm -fi Moose \
    WWW::Asana \
    Org::Parser \
    YAML
}

get-global-cpan-packages() {
  yes | cpanm -fi Carton
}

# ----------------------------------------------------------------------
# ### cpan ###
cpan-module-list() {
  perl -e "print \"@INC\"" | find -name "*.pm" -print
}

cpan-module-version() {
  perl -M$1 -le "print \$$1::VERSION"
}

cpan-uninstall() {
  perl -MConfig -MExtUtils::Install -e '($FULLEXT=shift)=~s{-}{/}g;uninstall "$Config{sitearchexp}/auto/$FULLEXT/.packlist",1'
}

alias cpanmini='cpan --mirror ~/.cpan/minicpan --mirror-only'
# alias cpan-uninstall='perl -MConfig -MExtUtils::Install -e '"'"'($FULLEXT=shift)=~s{-}{/}g;uninstall "$Config{sitearchexp}/auto/$FULLEXT/.packlist",1'"'"
