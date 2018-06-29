# Protobuf
get-protobuf() {
  case "${OSTYPE}" in
    freebsd* | darwin*) nix-install protobuf-2.6.1 ;;
    linux*)
      case $DIST_VERSION in
        14.04 | 16.04) ;;
        *) nix-install protobuf-2.6.1 ;;
      esac
      ;;
  esac
}
if ! type -p protoc >/dev/null; then get-protobuf; fi

# ----------------------------------------------------------------------
# Thrift
get-thrift() {
  case "${OSTYPE}" in
    freebsd* | darwin*) nix-install thrift-0.9.3 ;;
    linux*)
      case $DIST_VERSION in
        14.04 | 16.04) ;;
        *) nix-install thrift-0.9.3 ;;
      esac
      ;;
  esac
}
if ! type -p thrift >/dev/null; then get-thrift; fi
