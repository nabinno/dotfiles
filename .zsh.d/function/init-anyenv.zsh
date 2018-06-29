export PATH="$HOME/.anyenv/bin:$PATH"

get-anyenv() {
  case "${OSTYPE}" in
    freebsd* | darwin* | linux*)
      git clone https://github.com/riywo/anyenv ~/.anyenv
      eval "$(anyenv init -)"
      ;;
  esac
}
if ! type -p anyenv >/dev/null; then get-anyenv; fi
if type -p anyenv >/dev/null; then eval "$(anyenv init -)"; fi
