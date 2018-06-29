get-aws() {
  if ! type -p pip >/dev/null; then get-pip; fi
  case "${OSTYPE}" in
    freebsd*) ;;
    darwin* | linux*)
      pip install -U awscli s3cmd
      fix-compdef-problem
      ;;
  esac
}

fix-compdef-problem() {
  touch ~/.zshenv
  echo 'autoload -Uz compinit
compinit' >>~/.zshenv
}
if ! type -p aws >/dev/null; then get-aws; fi
