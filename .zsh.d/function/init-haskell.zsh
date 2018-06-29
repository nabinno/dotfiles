export REQUIRED_GHC_VERSION=8.4.3
# export REQUIRED_CABAL_VERSION=1.22.9.0

get-ghc() {
  case $OSTYPE in
    freebsd* | darwin*) nix-install ghc ;;
    linux*)
      case $DIST in
        Redhat* | RedHat* | Debian) nix-install ghc ;;
        Ubuntu*)
          case $DIST_VERSION in
            14.04) ;;
          esac
          ;;
      esac
      ;;
  esac
}

get-ghc-with-asdf() {
  case $OSTYPE in
    freebsd* | darwin* | linux*)
      asdf install haskell $REQUIRED_GHC_VERSION
      asdf global haskell $REQUIRED_GHC_VERSION
      ;;
  esac
}

get-cabal() {
  case $OSTYPE in
    freebsd* | darwin*)
      nix-install cabal-install
      cabal update
      ;;
    linux*)
      case $DIST in
        Redhat* | RedHat* | Debian)
          nix-install cabal-install
          cabal update
          ;;
        Ubuntu*)
          case $DIST_VERSION in
            14.04) ;;
          esac
          ;;
      esac
      ;;
  esac
}

if ! type -p ghc >/dev/null; then get-ghc && get-cabal; fi
if ! type -p cabal >/dev/null; then get-cabal; fi
