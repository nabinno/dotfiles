export REQUIRED_CRYSTAL_VERSION=0.25.0

get-crystal() {
  case $OSTYPE in
    linux*)
      asdf install rust $REQUIRED_CRYSTAL_VERSION
      asdf global rust $REQUIRED_CRYSTAL_VERSION
      ;;
  esac
}
if ! type -p crystal >/dev/null; then get-crystal; fi
