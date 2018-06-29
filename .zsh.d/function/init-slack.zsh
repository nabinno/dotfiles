get-slackchat() {
  case "${OSTYPE}" in
    darwin* | linux*)
      wget https://github.com/vektorlab/slackcat/releases/download/v0.7/slackcat-0.7-linux-amd64 -O ~/.local/bin/slackchat
      chmod +x ~/.local/bin/slackchat
      ;;
  esac
}
