REQUIRED_MACPORT_VERSION=2.3.3

get-base() {
  case "${OSTYPE}" in
    msys) ;;
    cygwin) ;;
    freebsd*) port install -y ruby ;;
    darwin*)
      curl -O https://distfiles.macports.org/MacPorts/MacPorts-$REQUIRED_MACPORT_VERSION.tar.bz2
      tar xf MacPorts-$REQUIRED_MACPORT_VERSION.tar.bz2
      cd MacPorts-$REQUIRED_MACPORT_VERSION/
      ./configure
      make
      sudo make install
      mkdir -pr /opt/mports
      cd /opt/mports
      sudo svn checkout https://svn.macports.org/repository/macports/trunk
      cd /opt/mports/trunk/base
      sudo ./configure --enable-readline
      sudo make
      sudo make install
      sudo make distclean
      brew install -y ruby
      ;;
    linux*)
      case "${DIST}" in
        Redhat | RedHat)
          wget http://dl.fedoraproject.org/pub/epel/7/x86_64/e/epel-release-7-9.noarch.rpm
          sudo rpm -ihv epel-release-7-9.noarch.rpm
          rm -f epel-release-7-9.noarch.rpm
          sudo yum update -y
          sudo yum install -y \
            bind-utils \
            gcc \
            gdbm-devel \
            git \
            htop \
            libffi-devel \
            libicu \
            libyaml-devel \
            mysql-devel \
            ncurses-devel \
            openssl-devel \
            readline-devel \
            ruby \
            screen \
            zlib-devel \
            curl
          ;;
        Debian | Ubuntu)
          sudo apt-get update -y
          case "${DIST_VERSION=}" in
            12.04) sudo apt-get install -y python-software-properties ;;
            14.04)
              sudo apt-get install -y \
                software-properties-common \
                python3-software-properties
              ;;
          esac
          sudo add-apt-repository -y ppa:fcwu-tw/ppa
          sudo add-apt-repository -y ppa:git-core/ppa
          sudo apt-get update -y
          case "${DIST_VERSION=}" in
            12.04 | 14.04)
              sudo apt-get install -y \
                apt-transport-https \
                automake \
                autotools-dev \
                base-files \
                base-passwd \
                binutils \
                build-essential \
                bzip2 \
                cmake \
                curl \
                dnsutils \
                gdb \
                git \
                git-core \
                gnupg \
                htop \
                imagemagick \
                libarchive-dev \
                libarchive12 \
                libbz2-1.0 \
                libbz2-dev \
                libc6 \
                libcurl3 \
                libcurl3-gnutls \
                libcurl4-openssl-dev \
                libdb5.1-dev \
                libevent-1.4-2 \
                libevent-core-1.4-2 \
                libevent-dev \
                libevent-extra-1.4-2 \
                libffi-dev \
                libgdbm-dev \
                libglib2.0-dev \
                libicu-dev \
                libldap-2.4-2 \
                libldap2-dev \
                libltdl-dev \
                libltdl7 \
                liblzma-dev \
                liblzma-doc \
                liblzma5 \
                libmagickcore-dev \
                libmagickwand-dev \
                libmysqlclient-dev \
                libncap-dev \
                libncap44 \
                libncurses-dev \
                libncurses5-dev \
                libncursesw5 \
                libncursesw5-dev \
                libpam0g-dev \
                libpcre3 \
                libpcre3-dev \
                libpng12-0 \
                libpng12-dev \
                libpq-dev \
                libqt4-dev \
                libreadline-dev \
                libreadline6-dev \
                libsndfile1-dev \
                libsqlite3-dev \
                libssl-dev \
                libssl0.9.8 \
                libxml2 \
                libxml2-dev \
                libxslt1-dev \
                libxt-dev \
                libxt6 \
                libyaml-dev \
                make \
                openssl \
                psmisc \
                ruby \
                s3cmd \
                sqlite3 \
                strace \
                telnet \
                tsconf \
                unzip \
                util-linux \
                wget \
                whiptail \
                xz-utils \
                zlib1g \
                zlib1g-dev \
                zip
              ;;
            16.04)
              sudo apt install -y \
                apt-transport-https \
                automake \
                autotools-dev \
                base-files \
                base-passwd \
                binutils \
                bison \
                build-essential \
                bzip2 \
                cmake \
                curl \
                dnsutils \
                gdb \
                git \
                git-core \
                gnupg \
                htop \
                imagemagick \
                libarchive-dev \
                libbz2-1.0 \
                libbz2-dev \
                libc6 \
                libcurl3 \
                libcurl3-gnutls \
                libcurl4-openssl-dev \
                libdb-dev \
                libevent-1.4-2 \
                libevent-core-1.4-2 \
                libevent-dev \
                libevent-extra-1.4-2 \
                libffi-dev \
                libgdbm-dev \
                libglib2.0-dev \
                libicu-dev \
                libldap-2.4-2 \
                libldap2-dev \
                libltdl7 \
                libltdl-dev \
                liblzma5 \
                liblzma-dev \
                liblzma-doc \
                libmagickcore-dev \
                libmagickwand-dev \
                libmysqlclient-dev \
                libncap44 \
                libncap-dev \
                libncurses5-dev \
                libncurses-dev \
                libncursesw5 \
                libncursesw5-dev \
                libpam0g-dev \
                libpcre3 \
                libpcre3-dev \
                libpng12-0 \
                libpng12-dev \
                libpq-dev \
                libqt4-dev \
                libreadline6-dev \
                libreadline-dev \
                libsndfile1-dev \
                libsqlite3-dev \
                libssl-dev \
                libxml2 \
                libxml2-dev \
                libxslt1-dev \
                libxt6 \
                libxt-dev \
                libyaml-dev \
                make \
                openssl \
                psmisc \
                re2c \
                ruby \
                s3cmd \
                sqlite3 \
                strace \
                telnet \
                unzip \
                util-linux \
                wget \
                whiptail \
                xz-utils \
                zip \
                zlib1g-dev
              ;;
          esac
          ;;
      esac
      ;;
  esac
}

get-wget() {
  case $OSTYPE in
    darwin*) brew install wget ;;
    freebsd*) port install wget ;;
    linux*)
      case "${DIST}" in
        Redhat | RedHat) sudo yum install -y wget ;;
        Debian | Ubuntu) sudo apt-get install -y wget ;;
      esac
      ;;
  esac
}

get-rlwrap() {
  if ! type -p wget >/dev/null; then get-wget; fi
  case "${OSTYPE}" in
    freebsd* | darwin*) ;;
    linux*)
      case "${DIST}" in
        Redhat | RedHat) sudo yum install -y rlwrap ;;
        Debian | Ubuntu) sudo apt-get install -y rlwrap ;;
      esac
      local current_pwd=$(pwd)
      mkdir -p ~/.local/rlwrap
      cd ~/.local/rlwrap
      wget http://www.linuxification.at/download/rlwrap-extensions-V12-0.01.tar.gz
      tar xvfz rlwrap-extensions-V12-0.01.tar.gz
      cd $current_pwd
      ;;
  esac
}

get-autoconf() {
  if ! type -p wget >/dev/null; then get-wget; fi
  case "${OSTYPE}" in
    freebsd*) ;;
    darwin*) ;;
    linux*)
      case "${DIST}" in
        Redhat | RedHat) gnu-get autoconf-2.69 ;;
        Debian | Ubuntu)
          wget http://ftp.debian.org/debian/pool/main/a/automake-1.14/automake_1.14.1-3_all.deb
          sudo dpkg -i automake_1.14.1-3_all.deb
          ;;
      esac
      ;;
  esac
}

get-boost() {
  if ! type -p wget >/dev/null; then get-wget; fi
  case "${OSTYPE}" in
    freebsd*) ;;
    darwin*) ;;
    linux*)
      case "${DIST}" in
        Redhat | RedHat)
          wget http://sourceforge.net/projects/boost/files/boost/1.53.0/boost_1_53_0.tar.gz
          tar xvf boost_1_53_0.tar.gz
          cd boost_1_53_0
          ./bootstrap.sh
          sudo ./b2 install
          cd ..
          rm -rf boost_1_53_0.tar.gz boost_1_53_0
          ;;
        Debian | Ubuntu)
          sudo apt-get install -y \
            libboost-dev \
            libboost-test-dev \
            libboost-program-options-dev \
            libboost-system-dev \
            libboost-filesystem-dev \
            libevent-dev
          ;;
      esac
      ;;
  esac
}

# gnu-get() {
#     typeset -A softwares
#     array=("${(@s:-:)1}")
#     version=$array[2]
#     softwares[$version]=$array[1]
#     if ! type -p wget > /dev/null ; then get-wget ; fi
#     if [ $# -ge 1 ] ; then
#         for k in ${(@k)softwares}; do
#             wget http://ftp.gnu.org/gnu/$softwares[$k]/$softwares[$k]-$k.tar.gz
#             tar xvf $softwares[$k]-$k.tar.gz
#             cd $softwares[$k]-$k
#             ./configure --prefix=/usr
#             make
#             sudo make install
#             cd ..
#             rm -fr $softwares[$k]-$k $softwares[$k]-$k.tar.gz
#         done
#     fi
# }
