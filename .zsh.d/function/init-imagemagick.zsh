export REQUIRED_IMAGEMAGICK_VERSION=6.9.7-6

function get-imagemagick() {
  case ${OSTYPE} in
    freebsd* | darwin*)
      nix-install imagemagick-${REQUIRED_IMAGEMAGICK_VERSION}
      ;;
    linux*)
      case $DIST in
        Redhat | RedHat)
          nix-install imagemagick-${REQUIRED_IMAGEMAGICK_VERSION}
          ;;
        Ubuntu | Debian)
          case $DIST_VERSION in
            12.04 | 14.04)
              nix-install imagemagick-${REQUIRED_IMAGEMAGICK_VERSION}
              ;;
            16.04)
              sudo apt install -y imagemagick-${REQUIRED_IMAGEMAGICK_VERSION}
              ;;
          esac
          ;;
      esac
      ;;
  esac
}
