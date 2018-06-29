export REQUIRED_RUST_VERSION=1.27.0
export PATH=$PATH:~/.cargo/bin

get-rust() {
  case $OSTYPE in
    linux*)
      asdf install rust $REQUIRED_RUST_VERSION
      ;;
  esac
}

get-global-cargo-packages() {
  cargo install \
    ripgrep \
    fselect
}
