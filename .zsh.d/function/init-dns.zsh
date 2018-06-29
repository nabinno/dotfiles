get-dnsmasq() {
  case $OSTYPE in
    linux*)
      case "${DIST}" in
        Redhat | RedHat | Debian) ;;
        Ubuntu)
          case $DIST_VERSION in
            12.04 | 14.04)
              sudo apt-get update
              sudo apt-get install dnsmasq
              ;;
            16.04)
              sudo apt update
              sudo apt install dnsmasq
              ;;
          esac
          ;;
      esac
      ;;
  esac
}
if ! type dnsmasq >/dev/null; then get-dnsmasq; fi

dnsmasq-restart() {
  case $OSTYPE in
    linux*)
      case "${DIST}" in
        Redhat | RedHat | Debian) ;;
        Ubuntu) sudo service dnsmasq restart ;;
      esac
      ;;
  esac
}

set-dnsmasq() {
  case $OSTYPE in
    linux*)
      case "${DIST}" in
        Redhat | RedHat | Debian) ;;
        Ubuntu)
          if [ -f /etc/dnsmasq.d/minikube.conf ]; then return; fi
          sudo mkdir /etc/dnsmasq.d
          sudo bash -c "echo address=/minikube.dev/127.0.0.1 > /etc/dnsmasq.d/minikube.conf"
          dnsmasq-restart
          ;;
      esac
      ;;
  esac
}
if type dnsmasq >/dev/null; then set-dnsmasq; fi

alias dmr=dnsmasq-restart
