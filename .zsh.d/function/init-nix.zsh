get-nix() {
  case "${OSTYPE}" in
    cygwin*)
      local REQUIRED_NIX_VERSION=1.11.2
      local CYGWIN_NAME=nix-on-cygwin
      export PKG_CONFIG_PATH=/usr/local/lib/pkgconfig:/usr/lib/pkgconfig:/usr/share/pkgconfig:/lib/pkgconfig
      # ### base installation ###
      curl http://nixos.org/releases/nix/nix-${REQUIRED_NIX_VERSION}/nix-${REQUIRED_NIX_VERSION}.tar.bz2 >nix-${REQUIRED_NIX_VERSION}.tar.bz2
      tar jxf nix-${REQUIRED_NIX_VERSION}.tar.bz2
      cd nix-${REQUIRED_NIX_VERSION}
      mkdir /nix
      ./configure
      make
      make install
      mkdir ~/.nixpkgs
      echo "{ allowBroken = true; }" >~/.nixpkgs/config.nix
      echo -e "nameserver 8.8.8.8\nnameserver 8.8.4.4" >/etc/resolv.conf
      cd ~
      rm -fr nix-${REQUIRED_NIX_VERSION}*
      # # ### OPTIONAL: needed for things like postgresql, mysql ###
      # CYGWIN_NAME=`basename \`cygpath -w /\` | cut -c 8-`
      # sed -i'' -e 's/\"CYGWIN cygserver\"/\"CYGWIN cygserver ${service_name}\"/' /usr/bin/cygserver-config
      # cygserver-config --yes --name $CYGWIN_NAME
      # cygrunsrv --start $CYGWIN_NAME
      # # ### OPTIONAL: if you need ssh connection to this ###
      # CYGWIN_NAME=`basename \`cygpath -w /\` | cut -c 8-`
      # ssh-host-config --yes --cygwin ntsec --name sshd-$CYGWIN_NAME --port 3000
      # # <enter password>
      # cygrunsrv --start sshd-$CYGWIN_NAME
      ;;
    darwin*)
      cd ~ && curl https://nixos.org/nix/install | sh
      sudo chown -R ${CURRENT_USER} /nix
      ;;
    linux*)
      case $DIST in
        Redhat | RedHat | Debian)
          cd ~ && curl https://nixos.org/nix/install | sh
          sudo chown -R ${CURRENT_USER} /nix
          ;;
        Ubuntu)
          case $DIST_VERSION in
            12.04)
              cd ~ && curl https://nixos.org/nix/install | sh
              nix-channel --add https://nixos.org/channels/nixpkgs-unstable
              nix-channel --update
              sudo groupadd -g 20000 nixbld
              for i in $(seq 1 10); do
                sudo useradd -u $(expr 20000 + $i) -g nixbld -c "Nix build user $i" -d /var/empty -s /noshell
              done
              sudo echo "build-users-group = nixbld" >>/etc/nix/nix.conf
              sudo chown -R ${CURRENT_USER} /nix
              source ~/.nix-profile/etc/profile.d/nix.sh
              ;;
            14.04 | 16.04)
              cd ~ && curl https://nixos.org/nix/install | sh
              sudo chown -R ${CURRENT_USER} /nix
              ;;
          esac
          ;;
      esac
      ;;
  esac
}

set-nix() {
  case "${OSTYPE}" in
    cygwin*)
      export NIXPKGS=~/.local/nixpkgs
      source /usr/local/etc/profile.d/nix.sh
      ;;
    darwin*) source ~/.nix-profile/etc/profile.d/nix.sh ;;
    linux*)
      case $DIST in
        Redhat | RedHat | Debian) source ~/.nix-profile/etc/profile.d/nix.sh ;;
        Ubuntu) source ~/.nix-profile/etc/profile.d/nix.sh ;;
      esac
      ;;
  esac
}

nix-install() {
  case "${OSTYPE}" in
    darwin*) nix-env --install $1 ;;
    linux*)
      case $DIST in
        Redhat | RedHat | Debian) nix-env --install $1 ;;
        Ubuntu) nix-env --install $1 ;;
      esac
      ;;
  esac
}

nix-uninstall() {
  nix-env --uninstall $1
}

nix-search() {
  nix-env -qa $1
}

nix-list() {
  nix-env -q
}

nix-list-versions() {
  nix-env -qc
}

nix-update() {
  nix-env --upgrade $1
}

nix-update-all() {
  nix-channel --update
}

get-nix-packages() {
  case "${OSTYPE}" in
    darwin* | linux*)
      nix-install phantomjs
      nix-install the_silver_searcher
      nix-install tree
      nix-install perf
      nix-install uuid
      nix-install jq
      ;;
  esac
}

case "${OSTYPE}" in
  cygwin*)
    if [ ! -f /usr/local/bin/nix-env ]; then get-nix; fi
    if [ -f /usr/local/bin/nix-env ]; then set-nix; fi
    ;;
  darwin*) ;;
  linux*)
    case $DIST in
      Ubuntu)
        case $DIST_VERSION in
          12.04 | 16.04)
            if [ ! -f ~/.nix-profile/bin/nix-env ]; then get-nix; fi
            if [ -f ~/.nix-profile/bin/nix-env ]; then set-nix; fi
            ;;
        esac
        ;;
    esac
    ;;
esac
