# 1. BasicSettings::OsDetect
# 1. BasicSettings::EnvironmentVariable
# 1. BasicSettings::EnvironmentVariable::Locale
# 1. BasicSettings::EnvironmentVariable::Local
# 1. BasicSettings::BaseInstallation
# 1. BasicSettings::PackageManager::WindowsManagementFramework
# 1. BasicSettings::PackageManager::WindowsManagementFramework::Chocolatey
# 1. BasicSettings::PackageManager::WindowsManagementFramework::Chocolatey::Pacman
# 1. BasicSettings::PackageManager::WindowsManagementFramework::Chocolatey::AptCyg
# 1. BasicSettings::PackageManager::Nix
# 1. BasicSettings::PackageManager::Chef
# 1. BasicSettings::PackageManager::Anyenv
# 1. BasicSettings::PackageManager::Docker
# 1. BasicSettings::PackageManager::Homebrew
# 1. BasicSettings::PackageManager::Autoparts
# 2. ProgrammingLanguage::Ruby
# 2. ProgrammingLanguage::Elixir
# 2. ProgrammingLanguage::Haskell
# 2. ProgrammingLanguage::Go
# 2. ProgrammingLanguage::DotNetFramework
# 2. ProgrammingLanguage::Java
# 2. ProgrammingLanguage::Php
# 2. ProgrammingLanguage::Python
# 2. ProgrammingLanguage::Perl
# 2. ProgrammingLanguage::Javascript
# 2. ProgrammingLanguage::RemoteProcedureCall
# 3. Daemon::Database::Postgresql
# 3. Daemon::Database::Mysql
# 3. Daemon::Database::Redis
# 3. Daemon::HttpServer::Nginx
# 4. IntegratedDevelopmentEnvironment::Emacs
# 4. IntegratedDevelopmentEnvironment::Emacs::Ctags
# 4. IntegratedDevelopmentEnvironment::Emacs::Pandoc
# 4. IntegratedDevelopmentEnvironment::ResourceManagement::Filesystem
# 4. IntegratedDevelopmentEnvironment::ResourceManagement::Git
# 4. IntegratedDevelopmentEnvironment::ResourceManagement::PlantUml
# 4. IntegratedDevelopmentEnvironment::SoftwareDebugging::Benchmark
# 4. IntegratedDevelopmentEnvironment::OsLevelVirtualization::Vagrant
# 4. IntegratedDevelopmentEnvironment::SoftwareDeployment::Terraform
# 4. IntegratedDevelopmentEnvironment::ComputerTerminal::Zsh
# 4. IntegratedDevelopmentEnvironment::ComputerTerminal::Zsh::Keybind
# 4. IntegratedDevelopmentEnvironment::ComputerTerminal::Zsh::Terminal
# 4. IntegratedDevelopmentEnvironment::ComputerTerminal::Zsh::Alias
# 4. IntegratedDevelopmentEnvironment::ComputerTerminal::Screen
# 4. IntegratedDevelopmentEnvironment::Chat::Slack
# 5. Platform::Heroku
# 5. Platform::GoogleCloudPlatform
# 5. Platform::AmazonWebServices
# 9. Other::Customized



# 1. BasicSettings::OsDetect
# --------------------------
local OS=
local KERNEL=
local DIST=
local DIST2=
local PSUEDONAME=
local REV=
local MACH=
local OSSTR=
local CURRENT_USER=$(whoami)
case "${OSTYPE}" in
    freebsd*|darwin*|linux*)
        [ -z "$PS1" ] && return
        stty -ixon
        stty start undef
        stty stop undef
        OS=`uname -s`
        REV=`uname -r`
        MACH=`uname -m`
        function GetVersionFromFile {
            VERSION=`cat $1 | tr "\n" ' ' | sed s/.*VERSION.*=\ // `
        }
        if [ "${OS}" = "SunOS" ] ; then
            OS=Solaris
            ARCH=`uname -p`
            OSSTR="${OS} ${REV}(${ARCH} `uname -v`)"
        elif [ "${OS}" = "AIX" ] ; then
            OSSTR="${OS} `oslevel` (`oslevel -r`)"
        elif [ "${OS}" = "Linux" ] ; then
            KERNEL=`uname -r`
            if [ -f /etc/redhat-release ] ; then
                DIST='RedHat'
                DIST_VERSION=''
                DIST2=''
                PSUEDONAME=`cat /etc/redhat-release | sed -e 's/.*(//' | sed -e 's/)//'`
                REV=`cat /etc/redhat-release | sed -e 's/.*release //' | sed -e 's/ .*//'`
            elif [ -f /etc/SUSE-release ] ; then
                DIST="SUSE"
                DIST_VERSION=''
                DIST2=`cat /etc/SUSE-release | tr "\n" ' '| sed -e 's/VERSION.*//'`
                PSUEDONAME=''
                REV=`cat /etc/SUSE-release | tr "\n" ' ' | sed -e 's/.*= //'`
            elif [ -f /etc/mandrake-release ] ; then
                DIST='Mandrake'
                DIST_VERSION=''
                DIST2=''
                PSUEDONAME=`cat /etc/mandrake-release | sed -e 's/.*(//' | sed -e 's/)//'`
                REV=`cat /etc/mandrake-release | sed -e 's/.*release //' | sed -e 's/ .*//'`
            elif [ -f /etc/debian_version ] ; then
                if ! type -p lsb_release > /dev/null; then
                    sudo apt-get install -y lsb-release
                fi
                DIST="$(lsb_release -i -s)"
                DIST_VERSION="$(lsb_release -s -r)"
                DIST2="Debian `cat /etc/debian_version`"
                PSUEDONAME=''
                REV=""
            fi
            if [ -f /etc/UnitedLinux-release ] ; then
                DIST="${DIST}[`cat /etc/UnitedLinux-release | tr "\n" ' ' | sed -e 's/VERSION.*//'`]"
            fi
            OSSTR="${OS} ${DIST} ${REV}(${PSUEDONAME} ${KERNEL} ${MACH})"
        fi
esac
case $OSTYPE in
    msys)
        function get-winpath {
            local winpath=
            winpath=$(sed -e 's|\\|/|g' <<< $*)
            winpath=$(sed -e 's|C:|/C|g' <<< $winpath)
            winpath=$(sed -e 's|Program Files (x86)|Progra~2|g' <<< $winpath)
            winpath=$(sed -e 's|Program Files|Progra~1|g' <<< $winpath)
            winpath=$(sed -e 's| |\\ |g' <<< $winpath)
            echo $winpath
        } ;;
    cygwin)
        function get-winpath {
            local winpath=
            winpath=$(sed -e 's|\\|/|g' <<< $*)
            winpath=$(sed -e 's|C:|/cygdrive/C|g' <<< $winpath)
            winpath=$(sed -e 's|Program Files (x86)|Progra~2|g' <<< $winpath)
            winpath=$(sed -e 's|Program Files|Progra~1|g' <<< $winpath)
            winpath=$(sed -e 's| |\\ |g' <<< $winpath)
            echo $winpath
        } ;;
esac


# 1. BasicSettings::EnvironmentVariable
# -------------------------------------
# export EDITOR=/usr/local/bin/vi
# export LANG=ja_JP.UTF-8
# export LANG=ja_JP.eucJP
export CLICOLOR=1
export EDITOR='vim -f'
export LANG=en_US.UTF-8
export LANGUAGE=en_US.UTF-8
export LC_ALL=en_US.UTF-8
export LC_CTYPE=UTF-8
export LC_MESSAGES=C
export MAILPATH=$HOME/MailBox/postmaster/maildir
export PATH=/usr/sbin:/sbin:$PATH
export PATH=$HOME/bin:$HOME/local/bin:$PATH
export PATH="/opt/local/bin:$PATH"
export PATH="/opt/local/sbin:$PATH"
export PATH="$HOME/.local/bin:$PATH"
export PS1="\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;36m\]\w\[\033[00m\]\$(parse_git_branch)\$ "


# 1. BasicSettings::Locale
# ------------------------
function get-ntp {
    case "${OSTYPE}" in
        linux*)
            case "${DIST}" in
                Redhat|RedHat) sudo yum install -y ntp ;;
                Debian|Ubuntu) sudo apt-get install -y ntp ;;
            esac
    esac
}
function set-locale {
    case "${OSTYPE}" in
        linux*)
            case "${DIST}" in
                Redhat|RedHat)
                    sudo mv /etc/localtime{,.org}
                    sudo ln -s /usr/share/zoneinfo/Japan /etc/localtime ;;
                Debian|Ubuntu)
                    sudo mv /etc/localtime{,.org}
                    sudo ln -s /usr/share/zoneinfo/Asia/Tokyo /etc/localtime ;;
            esac
    esac
}


# 1. BasicSettings::Local
# -----------------------
if [ ! -d ~/.local/bin ] ; then mkdir -p ~/.local/bin ; fi


# 1. BasicSettings::BaseInstallation
# ----------------------------------
REQUIRED_MACPORT_VERSION=2.3.3
function get-base {
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
            brew install -y ruby ;;
        linux*)
            case "${DIST}" in
                Redhat|RedHat)
                    sudo yum update -y
                    yum install -y \
                        screen \
                        ruby \
                        git \
                        libicu \
                        curl ;;
                Debian|Ubuntu)
                    sudo apt-get update -y
                    case "${DIST_VERSION=}" in
                        12.04) sudo apt-get install -y python-software-properties ;;
                        14.04)
                            sudo apt-get install -y \
                                 software-properties-common \
                                 python3-software-properties ;;
                    esac
                    sudo add-apt-repository -y ppa:fcwu-tw/ppa
                    sudo add-apt-repository -y ppa:git-core/ppa
                    sudo apt-get update -y
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
                         libarchive12 \
                         libarchive-dev \
                         libbz2-1.0 \
                         libbz2-dev \
                         libc6 \
                         libcurl3 \
                         libcurl3-gnutls \
                         libcurl4-openssl-dev \
                         libdb5.1 \
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
                         libssl0.9.8 \
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
                         ruby1.9.3 \
                         s3cmd \
                         sqlite3 \
                         telnet \
                         tsconf \
                         unzip \
                         util-linux \
                         wget \
                         whiptail \
                         xz-utils \
                         zip \
                         zlib1g \
                         zlib1g-dev ;;
            esac
    esac
}
function get-wget {
    case $OSTYPE in
        darwin*) brew install wget ;;
        freebsd*) port install wget ;;
        linux*)
            case "${DIST}" in
                Redhat|RedHat) sudo yum install -y wget ;;
                Debian|Ubuntu) sudo apt-get install -y wget ;;
            esac
    esac
}
function get-rlwrap {
    if ! type -p wget > /dev/null ; then get-wget ; fi
    case "${OSTYPE}" in
        freebsd*|darwin*) ;;
        linux*)
            case "${DIST}" in
                Redhat|RedHat) sudo yum install -y rlwrap ;;
                Debian|Ubuntu) sudo apt-get install -y rlwrap ;;
            esac
            local current_pwd=`pwd`
            mkdir -p ~/.local/rlwrap
            cd ~/.local/rlwrap
            wget http://www.linuxification.at/download/rlwrap-extensions-V12-0.01.tar.gz
            tar xvfz rlwrap-extensions-V12-0.01.tar.gz
            cd $current_pwd
    esac
}
function get-autoconf {
    if ! type -p wget > /dev/null ; then get-wget ; fi
    case "${OSTYPE}" in
        freebsd*) ;;
        darwin*) ;;
        linux*)
            case "${DIST}" in
                Redhat|RedHat) gnu-get autoconf-2.69 ;;
                Debian|Ubuntu)
                    wget http://ftp.debian.org/debian/pool/main/a/automake-1.14/automake_1.14.1-3_all.deb
                    sudo dpkg -i automake_1.14.1-3_all.deb
            esac
    esac
}
function get-boost {
    if ! type -p wget > /dev/null ; then get-wget ; fi
    case "${OSTYPE}" in
        freebsd*) ;;
        darwin*) ;;
        linux*)
            case "${DIST}" in
                Redhat|RedHat)
                    wget http://sourceforge.net/projects/boost/files/boost/1.53.0/boost_1_53_0.tar.gz
                    tar xvf boost_1_53_0.tar.gz
                    cd boost_1_53_0
                    ./bootstrap.sh
                    sudo ./b2 install
                    cd ..
                    rm -rf boost_1_53_0.tar.gz boost_1_53_0 ;;
                Debian|Ubuntu)
                    sudo apt-get install -y \
                         libboost-dev \
                         libboost-test-dev \
                         libboost-program-options-dev \
                         libboost-system-dev \
                         libboost-filesystem-dev \
                         libevent-dev ;;
            esac
    esac
}
function gnu-get {
    typeset -A softwares
    array=("${(@s:-:)1}")
    version=$array[2]
    softwares[$version]=$array[1]
    if ! type -p wget > /dev/null ; then get-wget ; fi
    if [ $# -ge 1 ] ; then
        for k in ${(@k)softwares}; do
            wget http://ftp.gnu.org/gnu/$softwares[$k]/$softwares[$k]-$k.tar.gz
            tar xvf $softwares[$k]-$k.tar.gz
            cd $softwares[$k]-$k
            ./configure --prefix=/usr
            make
            sudo make install
            cd ..
            rm -fr $softwares[$k]-$k $softwares[$k]-$k.tar.gz
        done
    fi
}


# 1. BasicSettings::PackageManager::WindowsManagementFramework
# ------------------------------------------------------------
function abstract-powershell {
    case "$OSTYPE" in
        msys)
            local args=$(echo $*)
            /c/Windows/System32/WindowsPowerShell/v1.0/powershell.exe -noprofile -executionpolicy bypass -command "$args" ;;
        cygwin)
            local args=$(echo $*)
            /cygdrive/c/Windows/System32/WindowsPowerShell/v1.0/powershell.exe -noprofile -executionpolicy bypass -command "$args" ;;
    esac
}
case "$OSTYPE" in
    msys|cygwin)
        function get-package         { abstract-powershell $0 $* }
        function get-packageprovider { abstract-powershell $0 $* }
        function find-package        { abstract-powershell $0 $* }
        function install-package     { abstract-powershell $0 $* }
        function uninstall-package   { abstract-powershell $0 $* }
        function get-wmf             { find-package && get-packageprovider -name chocolatey }
        function wmf-add             { get-packageprovider -name $1 }
        function wmf-install         { install-package $* }
        function wmf-uninstall       { uninstall-package $* }
        function wmf-search          { find-package $* }
        function get-global-wmf-packages {
            wmf-install FoxitReader
            wmf-install Gpg4win
	    wmf-install InkScape
	    wmf-install IrfanView
	    wmf-install WinSplitRevolution
	    wmf-install googledrive
	    wmf-install 7zip
	    wmf-install f.lux
	    wmf-install nodejs
	    wmf-install terminals
	    wmf-install vagrant
        }
esac


# 1. BasicSettings::PackageManager::WindowsManagementFramework::Chocolatey
# ------------------------------------------------------------------------
case $OSTYPE in
    msys|cygwin)
        export PATH=/c/ProgramData/Chocolatey/bin:$PATH
        function get-choco {
            cmd /c @powershell -NoProfile -ExecutionPolicy Bypass -Command "iex ((new-object net.webclient).DownloadString('https://chocolatey.org/install.ps1'))" && SET PATH=%PATH%;%ALLUSERSPROFILE%\chocolatey\bin
            # set ChocolateyPath C:\ProgramData\chocolatey\lib
        }
        if ! type -p choco > /dev/null ; then get-choco ; fi ;;
esac

# 1. BasicSettings::PackageManager::WindowsManagementFramework::Chocolatey::Pacman
# --------------------------------------------------------------------------------
case $OSTYPE in
    msys)
        function get-pacman {
            choco install msys2
            # pacman -Sy pacman
            # pacman -Syu
            # pacman -S zsh
        }
        function get-base-pacman-packages {
            pacman -S conemu git tar unzip make patch
            pacman -S mingw-w64-x86_64-toolchain
            # set ConEmuDir c:\tools\msys64\opt\bin
            # set ConEmuWorkDir %USERPROFILE\OneDrive
        }
        if ! type -p pacman > /dev/null ; then get-pacman && get-global-pacman-packages ; fi ;;
esac


# 1. BasicSettings::PackageManager::WindowsManagementFramework::Chocolatey::AptCyg
# --------------------------------------------------------------------------------
case $OSTYPE in
    cygwin)
        function get-aptcyg {
            choco install cygwin
            wget https://raw.githubusercontent.com/transcode-open/apt-cyg/master/apt-cyg -O /usr/local/bin/apt-cyg
            chmod 755 /usr/local/bin/apt-cyg
        }
        function get-base-aptcyg-packages {
            apt-cyg install \
                    base-cygwin \
                    base-files \
                    bash \
                    binutils \
                    cron \
                    ctags \
                    curl \
                    cygrunsrv \
                    cygutils \
                    cygwin \
                    cygwin-devel \
                    findutils \
                    gawk \
                    gcc-core \
                    gcc-g++ \
                    git \
                    gnuplot \
                    grep \
                    hostname \
                    info \
                    less \
                    libbz2-devel \
                    libcrypt-devel \
                    libcurl-devel \
                    liblzma-devel \
                    libsqlite3-devel \
                    make \
                    mintty \
                    openssh \
                    openssl-devel \
                    patch \
                    perl-DBD-SQLite \
                    perl-WWW-Curl \
                    ping \
                    pkg-config \
                    renameutils \
                    rsync \
                    run \
                    screen \
                    sed \
                    shutdown \
                    tar \
                    tree \
                    unzip \
                    util-linux \
                    vim \
                    wget \
                    which \
                    whois \
                    zip \
                    zoo \
                    zsh
        }
        if ! apt-cyg > /dev/null ; then get-aptcyg && get-base-aptcyg-packages ; fi ;;
esac


# 1. BasicSettings::PackageManager::Nix
# -------------------------------------
function get-nix {
    case "${OSTYPE}" in
        cygwin*)
            local REQUIRED_NIX_VERSION=1.11.2
            local CYGWIN_NAME=nix-on-cygwin
            export PKG_CONFIG_PATH=/usr/local/lib/pkgconfig:/usr/lib/pkgconfig:/usr/share/pkgconfig:/lib/pkgconfig
            # ### base installation ###
            curl http://nixos.org/releases/nix/nix-${REQUIRED_NIX_VERSION}/nix-${REQUIRED_NIX_VERSION}.tar.bz2 > nix-${REQUIRED_NIX_VERSION}.tar.bz2
            tar jxf nix-${REQUIRED_NIX_VERSION}.tar.bz2
            cd nix-${REQUIRED_NIX_VERSION}
            mkdir /nix
            ./configure
            make
            make install
            mkdir ~/.nixpkgs
            echo "{ allowBroken = true; }" > ~/.nixpkgs/config.nix
            echo -e "nameserver 8.8.8.8\nnameserver 8.8.4.4" > /etc/resolv.conf
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
            sudo chown -R ${CURRENT_USER} /nix ;;
        linux*)
            case $DIST in
                Redhat|RedHat|Debian)
                    cd ~ && curl https://nixos.org/nix/install | sh
                    sudo chown -R ${CURRENT_USER} /nix ;;
                Ubuntu)
                    case $DIST_VERSION in
                        12.04)
                            cd ~ && curl https://nixos.org/nix/install | sh
                            nix-channel --add https://nixos.org/channels/nixpkgs-unstable
                            nix-channel --update
                            sudo groupadd -g 20000 nixbld
                            for i in `seq 1 10` ; do
                                sudo useradd -u `expr 20000 + $i` -g nixbld -c "Nix build user $i" -d /var/empty -s /noshell
                            done
                            sudo echo "build-users-group = nixbld" >> /etc/nix/nix.conf
                            sudo chown -R ${CURRENT_USER} /nix
                            source ~/.nix-profile/etc/profile.d/nix.sh ;;
                    esac
            esac
    esac
}
function set-nix {
    case "${OSTYPE}" in
        cygwin*)
            export NIXPKGS=~/.local/nixpkgs
            source /usr/local/etc/profile.d/nix.sh ;;
        darwin*) source ~/.nix-profile/etc/profile.d/nix.sh ;;
        linux*)
            case $DIST in
                Redhat|RedHat|Debian) source ~/.nix-profile/etc/profile.d/nix.sh ;;
                Ubuntu) case $DIST_VERSION in (12.04) source ~/.nix-profile/etc/profile.d/nix.sh ;; esac
            esac
    esac
}
function nix-install {
    case "${OSTYPE}" in
        darwin*) nix-env --install $1 ;;
        linux*)
            case $DIST in
                Redhat|RedHat|Debian) nix-env --install $1 ;;
                Ubuntu) case $DIST_VERSION in (12.04) nix-env --install $1 ;; esac
            esac
    esac
}
function nix-uninstall     { nix-env --uninstall $1 }
function nix-search        { nix-env -qa $1 }
function nix-list          { nix-env -q }
function nix-list-versions { nix-env -qc }
function nix-update        { nix-env --upgrade $1 }
function get-nix-packages {
    case "${OSTYPE}" in
        darwin*|linux*)
            nix-install \
                heroku_toolbelt \
                phantomjs \
                the_silver_searcher \
                tree \
                perf \
                uuid ;;
    esac
}
case "${OSTYPE}" in
    cygwin*)
        if [ ! -f /usr/local/bin/nix-env ] ; then get-nix ; fi
        if [   -f /usr/local/bin/nix-env ] ; then set-nix ; fi ;;
    darwin*|linux*)
        if [ ! -f ~/.nix-profile/bin/nix-env ] ; then get-nix ; fi
        if [   -f ~/.nix-profile/bin/nix-env ] ; then set-nix ; fi ;;
esac


# 1. BasicSettings::PackageManager::Chef
# --------------------------------------
REQUIRED_CHEF_VERSION=0.17.9
function get-chef {
    case $OSTYPE in
        freebsd*|darwin*|linux*) curl https://omnitruck.chef.io/install.sh | sudo bash -s -- -c current -P chefdk -v $REQUIRED_CHEF_VERSION
    esac
}
function set-chef {
    case $OSTYPE in
        freebsd*|darwin*|linux*) eval "$(chef shell-init $SHELL)"
    esac
}
if ! type -p chef > /dev/null ; then get-chef ; fi
if   type -p chef > /dev/null ; then set-chef ; fi


# 1. BasicSettings::PackageManager::Anyenv
# ----------------------------------------
export PATH="$HOME/.anyenv/bin:$PATH"
function get-anyenv {
    case "${OSTYPE}" in
        freebsd*|darwin*|linux*)
            git clone https://github.com/riywo/anyenv ~/.anyenv
            eval "$(anyenv init -)" ;;
    esac
}
if ! type -p anyenv > /dev/null; then get-anyenv ; fi
if type -p anyenv > /dev/null; then eval "$(anyenv init -)" ; fi


# 1. BasicSettings::PackageManager::Docker
# ----------------------------------------
### setup ###
function get-docker {
    case "${OSTYPE}" in
        msys|cygwin) choco install boot2docker ;;
        freebsd*|darwin*) ;;
        linux*)
            case "${DIST}" in
                Redhat|RedHat)
                    sudo yum update && sudo yum install -y docker
                    sudo sed -i "s/^DOCKER_STORAGE_OPTIONS=/DOCKER_STORAGE_OPTIONS='--storage-opt dm.no_warn_on_loop_devices=true'/g" /etc/sysconfig/docker-storage
                    ;;
                Debian) sudo apt-get update; sudo apt-get install -y docker.io ;;
                Ubuntu)
                    sudo apt-key adv --keyserver hkp://pgp.mit.edu:80 --recv-keys 58118E89F3A912897C070ADBF76221572C52609D
                    if [ -f /etc/apt/sources.list.d/docker.list ]; then
                        sudo rm /etc/apt/sources.list.d/docker.list
                        sudo touch /etc/apt/sources.list.d/docker.list
                    fi
                    case "${DIST_VERSION}" in
                        12.04) sudo sh -c 'echo "deb https://apt.dockerproject.org/repo ubuntu-precise main" >> /etc/apt/sources.list.d/docker.list' ;;
                        14.04) sudo sh -c 'echo "deb https://apt.dockerproject.org/repo ubuntu-trusty main" >> /etc/apt/sources.list.d/docker.list' ;;
		    esac
                    sudo apt-get update
                    sudo apt-get purge lxc-docker*
                    sudo apt-cache policy docker-engine
		    sudo apt-get update
                    sudo apt-get install -y docker-engine ;;
            esac
    esac
}
case $OSTYPE in (linux) if ! type -p docker > /dev/null ; then get-docker ; fi ; esac
function docker-restart {
    case "${OSTYPE}" in
        darwin*)
            sudo service docker stop
            sudo service docker start
            sudo service docker status ;;
        linux*)
            case "${DIST}" in
                Redhat|RedHat)
                    sudo service docker stop
                    sudo service docker start
                    sudo service docker status ;;
                Debian|Ubuntu) ;;
            esac
    esac
}
function docker-status {
    case "${OSTYPE}" in
        darwin*) sudo service docker status ;;
        linux*)
            case "${DIST}" in
                Redhat|RedHat) sudo service docker status ;;
                Debian|Ubuntu) ;;
            esac
    esac
}
function get-docker-images {
    case "${OSTYPE}" in
        darwin*|linux*)
            get-sekka
            get-postgresql
            get-mysql
            get-redis
            get-memcached
            get-nginx
    esac
}
function set-docker-images {
    case "${OSTYPE}" in
        darwin*|linux*)
            sekka-restart
            postgresql-restart
            mysql-restart
            redis-restart
            memcached-restart
            nginx-restart
    esac
}
## ### alias ###
case $OSTYPE in
    msys)
        export DOCKER_PATH=$(get-winpath "C:\Program Files\Docker Toolbox")
        function boot2docker {
            local current_pwd=$(pwd)
            cd $DOCKER_PATH
            ./start.sh
            cd $current_pwd
        }
        alias docker="MSYS_NO_PATHCONV=1 $DOCKER_PATH/docker.exe"
        alias docker-compose="$DOCKER_PATH/docker-compose.exe"
        alias docker-machine="$DOCKER_PATH/docker-machine.exe"
        alias dcu="docker-machine start"
        alias dct="docker-machine stop"
        alias dcr="docker-machine restart"
        alias dcssh='docker-machine ssh'
        alias dcscp='docker-machine scp'
        alias dcp="ps aux | \grep -G 'docker.*'"
        alias dcs="docker-machine status"
        alias dd='docker-machine kill -f'
        alias ddel='docker-machine kill -f'
        alias dj='docker-machine ssh'
        alias dk='docker-machine rm -f'
        alias dkill='docker-machine rm -f'
        alias dl='docker-machine config ; docker-machine env'
        alias dls='docker-machine config ; docker-machine env'
        alias dp='docker-machine ls'
        alias dps='docker-machine ls'
        ;;
    *)
        alias docker='sudo docker'
        alias dcr="docker-restart"
        alias dcp="ps aux | \grep -G 'docker.*'"
        alias dcs="docker-status"
        alias dck="sudo killall docker"
        alias dc='docker commit $(docker ps -l -q)'
        alias dd='docker rmi -f'
        alias dda='docker rmi -f $(docker images -q)'
        alias ddel='docker rmi -f'
        alias ddela='docker rmi -f $(docker images -q)'
        function dh { docker history $1 | less -S }
        alias dj='docker run -i -t'
        alias dk='docker rm -f'
        alias dka='docker rm -f $(docker ps -a -q)'
        alias dkd='dka ; dda'
        alias dkill='docker rm -f'
        alias dkilla='docker rm -f $(docker ps -a -q)'
        alias dl='docker images'  # | less -S
        alias dls='docker images' # | less -S
        alias dp='docker ps -a'   # | less -S
        alias dps='docker ps -a'  # | less -S
        function dsshd { docker run -t -d -p 5000:3000 -P $1 /usr/sbin/sshd -D }
        alias dr='docker tag'
        alias dv='docker images -viz'
        function datach { docker start $1 ; docker atach $1 }
        function denv { docker run --rm $1 env }
        function dip {
            CI=$(docker ps -l -q)
            if [ $1 ] ; then
	        docker inspect --format {{.NetworkSettings.IPAddress}} $1
	        docker inspect --format {{.NetworkSettings.Ports}} $1
            else
	        docker inspect --format {{.NetworkSettings.IPAddress}} $CI
	        docker inspect --format {{.NetworkSettings.Ports}} $CI
            fi
        }
        function dnsenter {
            CI=$(docker ps -l -q)
            if [ $1 ] ; then
	        PID=$(docker inspect --format {{.State.Pid}} $1)
                nsenter --target $PID --mount --uts --ipc --net --pid
            else
	        PID=$(docker inspect --format {{.State.Pid}} $CI)
                nsenter --target $PID --mount --uts --ipc --net --pid
            fi
        }
        # ### docker compose / machine ###
        function get-docker-compose {
            case $OSTYPE in
                freebsd*|darwin*) nix-install docker-compose ;;
                linux*)
                    case $DIST in
                        Redhat|RedHat|Debian) nix-install docker-compose ;;
                        Ubuntu) case $DIST_VERSION in (12.04) nix-install docker-compose ;; esac
                    esac
            esac
        }
        function get-docker-machine {
            case "${OSTYPE}" in
                freebsd*|darwin*) ;;
                linux*)
                    case "${DIST}" in
                        Redhat|RedHat) ;;
                        Debian|Ubuntu)
                            wget https://github.com/docker/machine/releases/download/v0.1.0/docker-machine_linux-386 -O ~/.local/bin/docker-machine
                            chmod +x ~/.local/bin/docker-machine ;;
                    esac
            esac
        }
        if ! type -p docker-compose > /dev/null; then get-docker-compose ; fi
        if ! type -p docker-machine > /dev/null; then get-docker-machine ; fi
        # ### other ###
        case $DIST in
            Ubuntu)
                case $DIST_VERSION in
                    14.04) alias docker='DOCKER_HOST=tcp://:2375 docker'
                esac
        esac
        ;;
esac


# 1. BasicSettings::PackageManager::Homebrew
# ------------------------------------------
export MANPATH=$HOME/.linuxbrew/share/man:$MANPATH
export INFOPATH=$HOME/.linuxbrew/share/info:$INFOPATH
export LD_LIBRARY_PATH=$HOME/.linuxbrew/lib:$LD_LIBRARY_PATH
export PATH="$HOME/.linuxbrew/bin:$PATH"
function get-brew {
    case "${OSTYPE}" in
        darwin*) ;;
        linux*) ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/linuxbrew/go/install)"
    esac
}
function get-base-brew-packages {
    case "${OSTYPE}" in
        darwin*)
            brew install \
                 jq \
                 memcached \
                 the_silver_searcher \
                 tree ;;
        linux*)
            case "${DIST}" in
                Redhat|RedHat)
                    brew install \
                         jq \
                         tree ;;
                Debian|Ubuntu)
                    brew install \
                         jq ;;
            esac
    esac
}
if ! type -p brew > /dev/null ; then get-brew && get-base-brew-packages ; fi


# 1. BasicSettings::PackageManager::Autoparts
# -------------------------------------------
case "${OSTYPE}" in
    linux*) case "${DIST}" in
            Debian|Ubuntu)
                export PATH="$HOME/.parts/autoparts/bin:$PATH"
                export PATH="$HOME/.parts/lib/node_modules/less/bin:$PATH"
                function get-parts {
                    get-base
                    ruby -e "$(curl -fsSL https://raw.github.com/nitrous-io/autoparts/master/setup.rb)"
                    eval "$(parts env)"
                    get-parts-packages
                }
                function get-parts-packages {
                    parts install \
                          heroku_toolbelt \
                          phantomjs \
                          the_silver_searcher \
                          tree \
                          uuid
                }
                if ! type -p parts > /dev/null ; then ; get-parts ; fi
                if type -p parts > /dev/null ; then ; eval "$(parts env)" ; fi
        esac
esac


# 2. ProgrammingLanguage::Ruby
# ----------------------------
REQUIRED_RUBY_VERSION=2.2.0
REQUIRED_RUBY_VERSION_2=2.2.2
# ### version control ###
function get-rbenv {
    case "${OSTYPE}" in
        freebsd*|darwin*|linux*) anyenv install rbenv && exec -l zsh ;;
    esac
}
# ### installation ###
function get-ruby {
    case "${OSTYPE}" in
        cygwin) apt-cyg install ruby ;;
        freebsd*|darwin*|linux*)
            rbenv install $REQUIRED_RUBY_VERSION
            rbenv install $REQUIRED_RUBY_VERSION_2
            rbenv rehash
            rbenv global $REQUIRED_RUBY_VERSION
            get-global-gem-packages ;;
    esac
}
function get-global-gem-packages {
    gem install \
        bundler \
        rubygems-bundler \
        rails \
        # template engine
        compass \
        haml \
        slim \
        html2slim \
        # application server
        unicorn \
        sidekiq \
        # benchmark/profiling
        benchmark-ips \
        stackprof \
        rblineprof \
        peek-rblineprof \
        rack-lineprof \
        # other
        git-trend
}
if ! type -p rbenv > /dev/null; then
    get-rbenv ;
else
    if ! type -p ruby > /dev/null; then
        get-ruby
    else
        rm -f ~/.ruby-version
        rbenv global $REQUIRED_RUBY_VERSION
        _REQUIRED_RUBY_VERSION=$(echo $REQUIRED_RUBY_VERSION | sed 's/\(.*\..*\)\..*/\1/')
        _CURRENT_RUBY_VERSION=$(ruby -v | cut -f 2 -d " " | sed 's/^\([0-9]\{1,\}\.[0-9]\{1,\}\)\..*/\1/')
        if [[ $_REQUIRED_RUBY_VERSION > $_CURRENT_RUBY_VERSION ]]; then get-ruby; fi
    fi
fi


# 2. ProgrammingLanguage::Elixir
# ------------------------------
REQUIRED_ERLANG_VERSION=18.3
REQUIRED_ELIXIR_VERSION=1.2.0
REQUIRED_PHOENIXFRAMEWORK_VERSION=1.2.0
export PATH="$HOME/.local/exenv/bin:$PATH"
# ### version control ###
function get-kerl {
    case "${OSTYPE}" in
        freebsd*|darwin*|linux*)
            curl https://raw.githubusercontent.com/yrashk/kerl/master/kerl -o ~/.local/bin/kerl
            chmod a+x ~/.local/bin/kerl
            echo 'KERL_CONFIGURE_OPTIONS="--disable-hipe --enable-smp-support --enable-threads --enable-kernel-poll"' > ~/.kerlrc
    esac
}
if ! type -p kerl > /dev/null ; then get-kerl ; fi
function get-exenv {
    case "${OSTYPE}" in
        freebsd*|darwin*|linux*) anyenv install exenv && exec -l zsh
    esac
}
if ! type -p exenv > /dev/null ; then get-exenv ; fi
# ### installation ###
function get-erlang {
    case "${OSTYPE}" in
        freebsd*|darwin*|linux*)
            mkdir -p ~/.local/otp
            kerl build $REQUIRED_ERLANG_VERSION $REQUIRED_ERLANG_VERSION
            kerl install $REQUIRED_ERLANG_VERSION ~/.local/otp/$REQUIRED_ERLANG_VERSION
            source ~/.local/otp/$REQUIRED_ERLANG_VERSION/activate ;;
    esac
}
if [ ! -f ~/.local/otp/$REQUIRED_ERLANG_VERSION/activate ] ; then get-erlang ; fi
if [ -f ~/.local/otp/$REQUIRED_ERLANG_VERSION/activate ] ; then source ~/.local/otp/$REQUIRED_ERLANG_VERSION/activate ; fi
function get-elixir {
    case "${OSTYPE}" in
        freebsd*|darwin*|linux*)
            exenv install $REQUIRED_ELIXIR_VERSION
            exenv rehash
            exenv global $REQUIRED_ELIXIR_VERSION
            get-mix-packages ;;
    esac
}
function get-mix-packages {
    mix local.hex
    mix archive.install https://github.com/phoenixframework/archives/raw/master/phoenix_new-$REQUIRED_PHOENIXFRAMEWORK_VERSION.ez
}
if ! type -p iex > /dev/null ; then get-elixir ; fi
function get-ex_top {
    case "${OSTYPE}" in
        freebsd*|darwin*|linux*)
            cd ~
            git clone https://github.com/utkarshkukreti/ex_top
            cd ex_top && mix escript.build && cp -fr ./ex_top ~/.local/bin/
            cd ~ && rm -fr ex_top ;;
    esac
}
if ! type -p ex_top > /dev/null ; then get-ex_top ; fi


# 2. ProgrammingLanguage::Haskell
# -------------------------------
export REQUIRED_GHC_VERSION=7.10.3
export REQUIRED_CABAL_VERSION=1.22.9.0
function get-ghc {
    case $OSTYPE in
        freebsd*|darwin*) nix-install ghc-${REQUIRED_GHC_VERSION} ;;
        linux*)
            case $DIST in
                Redhat*|RedHat*|Debian) nix-install ghc-${REQUIRED_GHC_VERSION} ;;
                Ubuntu*)
                    case $DIST_VERSION in
                        14.04)  ;;
                    esac
            esac
    esac
}
function get-cabal {
    case $OSTYPE in
        freebsd*|darwin*)
            nix-install cabal-install-${REQUIRED_CABAL_VERSION}
            cabal update ;;
        linux*)
            case $DIST in
                Redhat*|RedHat*|Debian)
                    nix-install cabal-install-${REQUIRED_CABAL_VERSION}
                    cabal update ;;
                Ubuntu*)
                    case $DIST_VERSION in
                        14.04) ;;
                    esac
            esac
    esac
}
if ! type -p ghc > /dev/null ; then get-ghc && get-cabal ; fi
if ! type -p cabal > /dev/null ; then get-cabal ; fi


# 2. ProgrammingLanguage::Go
# --------------------------
export REQUIRED_GO_VERSION=1.4.2
export GOPATH=~/.go.d
export PATH="$HOME/.go.d/bin:$PATH"
# ### version control ###
function get-goenv {
    case "${OSTYPE}" in
        freebsd*|darwin*|linux*) anyenv install goenv && exec -l zsh ;;
    esac
}
if ! type -p goenv > /dev/null ; then get-goenv ; fi
function get-go {
    case "${OSTYPE}" in
        freebsd*|darwin*) ;;
        linux*) goenv install $REQUIRED_GO_VERSION ;;
    esac
}
if ! type -p go > /dev/null ; then get-go ; fi
function set-go {
    case "${OSTYPE}" in
        freebsd*|darwin*|linux*)
            goenv global $REQUIRED_GO_VERSION > /dev/null ;;
    esac
}
if type -p go > /dev/null ; then set-go ; fi
function get-global-go-packages {
    case "${OSTYPE}" in
        freebsd*|darwin*|linux*)
            go get github.com/peco/peco/cmd/peco
    esac
}


# 2. ProgrammingLanguage::DotNetFramework
# ---------------------------------------
REQUIRED_DOTNETFRAMEWORK_VERSION=4.0.30319
REQUIRED_MONO_VERSION=4.0.4.1
REQUIRED_NUNIT_VERSION=3.2.1
export DOTNET_HOME=$HOME/.local/dotnet
export PATH=$PATH:$DOTNET_HOME
export PATH="$HOME/.local/NUnit/bin:$PATH"
case "${OSTYPE}" in
    msys)
        export PATH="/C/Windows/Microsoft.NET/Framework64/v${REQUIRED_DOTNETFRAMEWORK_VERSION}:$PATH"
        export PATH=/C/Program~2/Mono/bin:$PATH ;;
    cygwin)
        export PATH="/cygdrive/C/Windows/Microsoft.NET/Framework64/v${REQUIRED_DOTNETFRAMEWORK_VERSION}:$PATH"
        export PATH=/cygdrive/C/Program~2/Mono/bin:$PATH ;;
esac
# A. Compiler        - Mono
# B. Task Runner     - DNVM / Dotnet CLI
# C. Package Manager - NuGet
# D. Scaffolding     - Yeoman / Grunt-init
# E. Complition      - OmniSharp
# F. Test Runner     - NUnit
# ### A. Compiler ###
function get-mono {
    case "${OSTYPE}" in
        msys|cygwin) choco install mono ;;
        freebsd*|darwin*) ;;
        linux*)
            case $DIST in
                Redhat*|RedHat*)
                    sudo yum install -y yum-utils
                    sudo rpm --import "http://keyserver.ubuntu.com/pks/lookup?op=get&search=0x3FA7E0328081BFF6A14DA29AA6A19B38D3D831EF"
                    sudo yum-config-manager --add-repo http://download.mono-project.com/repo/centos/
                    sudo yum install -y \
                         mono \
                         mono-complete ;;
                Debian|Ubuntu*)
                    case $DIST_VERSION in
                        14.04)
                            sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 3FA7E0328081BFF6A14DA29AA6A19B38D3D831EF
                            echo "deb http://download.mono-project.com/repo/debian wheezy main" | sudo tee /etc/apt/sources.list.d/mono-xamarin.list
                            sudo apt-get update
                            sudo apt-get install -y \
                                 mono \
                                 mono-complete ;;
                    esac
            esac ;;
    esac
}
# ### B. Task Runner ###
function get-dnvm {
    curl -sSL https://raw.githubusercontent.com/aspnet/Home/dev/dnvminstall.sh | DNX_BRANCH=dev sh
    source ~/.dnx/dnvm/dnvm.sh
    dnvm upgrade -u
}
function get-dotnetcli {
    case "${OSTYPE}" in
        msys)
            wget https://dotnetcli.blob.core.windows.net/dotnet/beta/Binaries/Latest/dotnet-dev-rhel-x64.latest.tar.gz
            pacman -S libicu libuuid libcurl openssl libunwind
            mkdir ~/.local/dotnet
            tar xf dotnet-dev-rhel-x64.latest.tar.gz -C ~/.local/dotnet --verbose
            rm -fr dotnet-dev-rhel-x64.latest.tar.gz ;;
        cygwin)
            # ### dotnet ###
            wget https://dotnetcli.blob.core.windows.net/dotnet/beta/Binaries/Latest/dotnet-dev-rhel-x64.latest.tar.gz
            apt-cyg install -y libicu libuuid libcurl openssl libunwind
            mkdir ~/.local/dotnet
            tar xf dotnet-dev-rhel-x64.latest.tar.gz -C ~/.local/dotnet --verbose
            rm -fr dotnet-dev-rhel-x64.latest.tar.gz ;;
        freebsd*|darwin*) ;;
        linux*)
            case $DIST in
                Redhat*|RedHat*)
                    # ### mono ###
                    nix-install \
                        dotnetbuildhelpers \
                        mono-${REQUIRED_MONO_VERSION} \
                        mono-addins-1.2 \
                        mono-dll-fixer \
                        mono-zeroconf-0.9.0
                    # ### dnvm ###
                    curl -sSL https://raw.githubusercontent.com/aspnet/Home/dev/dnvminstall.sh | DNX_BRANCH=dev sh
                    source ~/.dnx/dnvm/dnvm.sh
                    dnvm upgrade -u
                    # ### dotnet ###
                    wget https://dotnetcli.blob.core.windows.net/dotnet/beta/Binaries/Latest/dotnet-dev-rhel-x64.latest.tar.gz
                    sudo yum install -y libicu libuuid libcurl openssl libunwind
                    mkdir ~/dotnet
                    tar xf dotnet-dev-rhel-x64.latest.tar.gz -C ~/dotnet --verbose
                    rm -fr dotnet-dev-rhel-x64.latest.tar.gz ;;
                Debian|Ubuntu*)
                    case $DIST_VERSION in
                        14.04)
                            # ### dnvm ###
                            curl -sSL https://raw.githubusercontent.com/aspnet/Home/dev/dnvminstall.sh | DNX_BRANCH=dev sh
                            source ~/.dnx/dnvm/dnvm.sh
                            dnvm upgrade -u
                            # ### dotnet ###
                            sudo sh -c 'echo "deb [arch=amd64] http://apt-mo.trafficmanager.net/repos/dotnet/ trusty main" > /etc/apt/sources.list.d/dotnetdev.list'
                            sudo apt-key adv --keyserver apt-mo.trafficmanager.net --recv-keys 417A0893
                            sudo apt-get update
                            sudo apt-get install dotnet=1.0.0.001598-1 ;;
                    esac
            esac ;;
    esac
}
function set-dnvm {
    source ~/.dnx/dnvm/dnvm.sh
}
function set-mono {
    case "${OSTYPE}" in
        msys|cygwin)
            function csc         { cmd /c "csc.exe $*" }
            function vbc         { cmd /c "vbc.exe $*" }
            function jsc         { cmd /c "jsc.exe $*" }
            function msbuild     { cmd /c "MSBuild.exe $*" }
            function installutil { cmd /c "InstallUtil.exe $*" }
            function ngen        { cmd /c "ngen.exe $*" }
    esac
}
# ### C. Package Manager ###
function get-nuget {
    mkdir -fr ~/.local/NuGet
    wget https://dist.nuget.org/win-x86-commandline/latest/nuget.exe -O ~/.local/NuGet/nuget.exe
    wget http://headsigned.com/download/running-nuget-command-line-on-linux/Microsoft.Build.zip
    unzip Microsoft.Build.zip -d ~/.local/NuGet/
    rm -f Microsoft.Build.zip
}
function set-nuget {
    case $OSTYPE in
        msys|cygwin) function nuget { cmd /c "~/.local/NuGet/nuget.exe $*" } ;;
        linux*) alias nuget='mono ~/.local/NuGet/nuget.exe' ;;
    esac
}
# ### D. Scaffolding ###
function get-generator-dotnet {
    npm i -g \
        yo \
        generator-aspnet \
        grunt-init
    git clone https://github.com/nosami/grunt-init-csharpsolution.git ~/.grunt-init/csharpsolution
}
case "${OSTYPE}" in
    msys|cygwin)             if ! type -p csc > /dev/null ; then get-mono ; fi ;;
    freebsd*|darwin*) ;;
    linux*)
        case $DIST_VERSION in
            14.04) ;;
            *) if ! type -p mcs > /dev/null ; then get-mono ; fi ;;
        esac
esac
if [ ! -f ~/.dnx/dnvm/dnvm.sh ] ; then get-dnvm ;      fi
if [   -f ~/.dnx/dnvm/dnvm.sh ] ; then set-dnvm ;      fi
if ! type -p dotnet > /dev/null ; then get-dotnetcli ; fi
if [ ! -f ~/.local/NuGet/nuget.exe ] ; then get-nuget ; fi
if [   -f ~/.local/NuGet/nuget.exe ] ; then set-nuget ; fi
# ### E. Complition ###
function get-omnisharp {
    # ### omnisharp-roslyn (beta) ###
    # git clone https://github.com/OmniSharp/omnisharp-roslyn -b master
    # cd omnisharp-roslyn
    # case "${OSTYPE}" in
    #     msys|cygwin) cmd /c './build.ps1' ;;
    #     freebsd*|darwin*|linux*) bash ./build.sh ;;
    # esac
    # cd ..
    # rm -fr omnisharp-roslyn
    # ### omnisharp-server ###
    git clone https://github.com/OmniSharp/omnisharp-server.git
    cd omnisharp-server
    git submodule update --init --recursive
    case "${OSTYPE}" in (cygwin) cp -f OmniSharp/config-cygwin.json OmniSharp/config.json ;; esac
    case "${OSTYPE}" in
        msys|cygwin) msbuild ;;
        freebsd*|darwin*|linux*) xbuild ;;
    esac
    cd ..
    mv omnisharp-server ~/.local/
}
function set-omnisharp {
    case "${OSTYPE}" in
        msys|cygwin)
            function omnisharp { cmd /c "~/.local/omnisharp-server/OmniSharp/bin/Debug/OmniSharp.exe $*" } ;;
        freebsd*|darwin*|linux*)
            alias omnisharp='mono ~/.local/omnisharp-server/OmniSharp/bin/Debug/OmniSharp.exe' ;;
    esac
}
case "${OSTYPE}" in
    msys|cygwin|freebsd*|darwin*)
        if [ ! -f ~/.local/omnisharp-server/OmniSharp/bin/Debug/OmniSharp.exe ] ; then get-omnisharp ; fi
        if [   -f ~/.local/omnisharp-server/OmniSharp/bin/Debug/OmniSharp.exe ] ; then set-omnisharp ; fi ;;
    linux*)
        case $DIST_VERSION in
            14.04) ;;
            *)
                if [ ! -f ~/.local/omnisharp-server/OmniSharp/bin/Debug/OmniSharp.exe ] ; then get-omnisharp ; fi
                if [   -f ~/.local/omnisharp-server/OmniSharp/bin/Debug/OmniSharp.exe ] ; then set-omnisharp ; fi ;;
        esac
esac
# ### F. Test Runner ###
function get-nunit {
    wget https://github.com/nunit/nunit/releases/download/${REQUIRED_NUNIT_VERSION}/NUnit-${REQUIRED_NUNIT_VERSION}.zip
    unzip NUnit-${REQUIRED_NUNIT_VERSION}.zip -d ~/.local/NUnit
    rm -f NUnit-${REQUIRED_NUNIT_VERSION}.zip
}
function set-nunit {
    case "${OSTYPE}" in
        msys|cygwin)
            function nunit-console { cmd /c "~/.local/NUnit/bin/nunit3-console.exe $*" } ;;
        freebsd*|darwin*|linux*)
            alias nunit-console='mono ~/.local/NUnit/bin/nunit3-console.exe' ;;
    esac
}
case "${OSTYPE}" in
    msys|cygwin|freebsd*|darwin*|linux*)
        if [ ! -f ~/.local/NUnit/bin/nunit3-console.exe ] ; then get-nunit ; fi
        if [   -f ~/.local/NUnit/bin/nunit3-console.exe ] ; then set-nunit ; fi ;;
esac


# 2. ProgrammingLanguage::Java
# ----------------------------
REQUIRED_OPENJDK_VERSION=8u92b14
REQUIRED_OEPNJDK_SHORT_VERSION=8
case $DIST_VERSION in
    14.04) REQUIRED_OEPNJDK_SHORT_VERSION=system ;;
    *) REQUIRED_OEPNJDK_SHORT_VERSION=8 ;;
esac
REQUIRED_PLAY_VERSION=2.2.3
export PLAY_HOME=/usr/local/play-$REQUIRED_PLAY_VERSION
export PATH="$PLAY_HOME:$PATH"
case $OSTYPE in
    msys|cygwin)
        export JAVA_HOME=$(get-winpath $JAVA_HOME)
        export PATH=$JAVA_HOME/bin:$PATH ;;
esac
# ### version control ###
function get-jenv {
    case "${OSTYPE}" in
        freebsd*|darwin*|linux*) anyenv install jenv && exec -l zsh ;;
    esac
}
if ! type -p jenv > /dev/null ; then get-jenv ; fi
# ### installation ###
function get-java {
    case "${OSTYPE}" in
        freebsd*|darwin*)
            nix-install openjdk-$REQUIRED_OPENJDK_VERSION
            jenv add ~/.nix-profile/lib/openjdk
            jenv global $REQUIRED_OEPNJDK_SHORT_VERSION ;;
        linux*)
            case "${DIST}" in
                Redhat|RedHat|Debian)
                    nix-install openjdk-$REQUIRED_OPENJDK_VERSION
                    jenv add ~/.nix-profile/lib/openjdk
                    jenv global $REQUIRED_OEPNJDK_SHORT_VERSION ;;
                Ubuntu)
                    case $DIST_VERSION in
                        12.04)
                            sudo add-apt-repositoryppa:openjdk-r/ppa
                            sudo apt-get update && sudo apt-get install -y openjdk-8-jdk
                            jenv add /usr/lib/jvm/java-1.8.0-openjdk-amd64/
                            jenv global $REQUIRED_OEPNJDK_SHORT_VERSION ;;
                        14.04)
                            nix-install openjdk-$REQUIRED_OPENJDK_VERSION
                            jenv add /usr/lib/jvm/java
                            jenv global $REQUIRED_OEPNJDK_SHORT_VERSION ;;
                    esac
            esac
    esac
}
function set-javahome {
    case "${OSTYPE}" in
        freebsd*|darwin*)
            export JAVA_HOME=~/.nix-profile/lib/openjdk
            jenv global $REQUIRED_OEPNJDK_SHORT_VERSION ;;
        linux*)
            case "${DIST}" in
                Redhat|RedHat|Debian)
                    export JAVA_HOME=~/.nix-profile/lib/openjdk
                    jenv global $REQUIRED_OEPNJDK_SHORT_VERSION ;;
                Ubuntu)
                    case $DIST_VERSION in
                        12.04)
                            export JAVA_HOME=/usr/lib/jvm/java-1.8.0-openjdk-amd64/
                            jenv global $REQUIRED_OEPNJDK_SHORT_VERSION ;;
                        14.04)
                            export JAVA_HOME=/usr/lib/jvm/java/
                            jenv global $REQUIRED_OEPNJDK_SHORT_VERSION ;;
                    esac
            esac
    esac
}
function get-play {
    case "${OSTYPE}" in
        freebsd*|darwin*|linux*)
            wget http://downloads.typesafe.com/play/$REQUIRED_PLAY_VERSION/play-$REQUIRED_PLAY_VERSION.zip
            unzip play-$REQUIRED_PLAY_VERSION.zip
            mv play-$REQUIRED_PLAY_VERSION ~/.local/
            rm -fr play-$REQUIRED_PLAY_VERSION.zip ;;
    esac
}
function get-sbt {
    case "${OSTYPE}" in
        freebsd*|darwin*) sudo port install sbt ;;
        linux*)
            case "${DIST}" in
                Redhat|RedHat)
                    curl https://bintray.com/sbt/rpm/rpm | sudo tee /etc/yum.repos.d/bintray-sbt-rpm.repo
                    sudo yum install -y sbt ;;
                Debian|Ubuntu)
                    echo "deb https://dl.bintray.com/sbt/debian /" | sudo tee -a /etc/apt/sources.list.d/sbt.list
                    sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv 642AC823
                    sudo apt-get update
                    sudo apt-get install -y sbt ;;
            esac
    esac
}
if ! type -p java > /dev/null ; then get-java && set-javahome ; fi
if type -p java > /dev/null ; then set-javahome ; fi
if [ -d ~/.local/play-$REQUIRED_PLAY_VERSION ] ; then get-play && get-sbt ; fi


# 2. ProgrammingLanguage::Php
# ---------------------------
# ### version control ###
REQUIRED_PHP_VERSION=5.6.20
function get-phpenv {
    case "${OSTYPE}" in
        freebsd*|darwin*|linux*) anyenv install phpenv && exec -l zsh ;;
    esac
}
if ! type -p phpenv > /dev/null ; then get-phpenv ; fi
# ### installation ###
function get-php {
    case "${OSTYPE}" in
        freebsd*|darwin*) php-$REQUIRED_PHP_VERSION ;;
        linux*)
            case $DIST_VERSION in
                14.04) sudo apt-get install php-$REQUIRED_PHP_VERSION ;;
                *) nix-install php-$REQUIRED_PHP_VERSION ;;
            esac
    esac
}
if ! type -p php > /dev/null; then get-php; fi
function get-fastcgi {
    case "${OSTYPE}" in
        linux*)
            case "${DIST}" in
                Redhat|RedHat)
                    sudo yum install -y php-fpm
                    chkconfig php-fpm on ;;
                Ubuntu)
                    case "${DIST_VERSION}" in
                        12.04)
                            php_fastcgi_file='#!/bin/bash
FASTCGI_USER=www-data
FASTCGI_GROUP=www-data
ADDRESS=127.0.0.1
PORT=10003
PIDFILE=/var/run/php5-fpm.pid
CHILDREN=6
PHP5=/usr/sbin/php5-fpm
/usr/bin/spawn-fcgi -a $ADDRESS -p $PORT_DBDIR -P $PIDFILE -C $CHILDREN -u $FASTCGI_USER -g $FASTCGI_GROUP -f $PHP5'
                            php_fastcgid_file='#!/bin/bash
PHP_SCRIPT=/usr/bin/php-fastcgi
FASTCGI_USER=www-data
FASTCGI_GROUP=www-data
PID_DIR=/var/run/php-fastcgi
PID_FILE=/var/run/php-fastcgi/php-fastcgi.pid
RET_VAL=0
case "$1" in
    start)
        if [[ ! -d $PID_DIR ]]
        then
            mkdir $PID_DIR
            chown $FASTCGI_USER:$FASTCGI_GROUP $PID_DIR
            chmod 0770 $PID_DIR
        fi
        if [[ -r $PID_FILE ]]
        then
            echo "php-fastcgi already running with PID `cat $PID_FILE`"
            RET_VAL=1
        else
            $PHP_SCRIPT
            RET_VAL=$?
        fi
        ;;
    stop)
        if [[ -r $PID_FILE ]]
        then
            kill `cat $PID_FILE`
            rm $PID_FILE
            RET_VAL=$?
        else
            echo "Could not find PID file $PID_FILE"
            RET_VAL=1
        fi
        ;;
    restart)
        if [[ -r $PID_FILE ]]
        then
            kill `cat $PID_FILE`
            rm $PID_FILE
            RET_VAL=$?
        else
            echo "Could not find PID file $PID_FILE"
        fi
        $PHP_SCRIPT
        RET_VAL=$?
        ;;
    status)
        if [[ -r $PID_FILE ]]
        then
            echo "php-fastcgi running with PID `cat $PID_FILE`"
            RET_VAL=$?
        else
            echo "Could not find PID file $PID_FILE, php-fastcgi does not appear to be running"
        fi
        ;;
    *)
        echo "Usage: php-fastcgi {start|stop|restart|status}"
        RET_VAL=1
        ;;
esac
exit $RET_VAL'
                            sudo apt-get update -y
                            sudo apt-get install -y \
                                 nginx \
                                 php5-cli \
                                 spawn-fcgi \
                                 psmisc
                            sudo rm -fr /usr/bin/php-fastcgi
                            sudo rm -fr /etc/init.d/php-fastcgi
                            echo $php_fastcgi_file | sudo tee --append /usr/bin/php-fastcgi
                            echo $php_fastcgid_file | sudo tee --append /etc/init.d/php-fastcgi
                            sudo chmod +x /usr/bin/php-fastcgi
                            sudo chmod +x /etc/init.d/php-fastcgi
                            sudo update-rc.d php-fastcgi defaults ;;
                        14.04)
                            sudo apt-get update -y
                            sudo apt-get install -y \
                                 nginx \
                                 php5-cli \
                                 spawn-fcgi \
                                 psmisc ;;
                    esac
            esac
    esac
}
if ! type -p spawn-fcgi > /dev/null ; then get-fastcgi; fi
function php-fastcgid {
    case "${OSTYPE}" in
        darwin*) ;;
        linux*)
            case "${DIST}" in
                Redhat|RedHat) sudo /etc/init.d/php-fpm $1 ;;
                Ubuntu)
                    case "${DIST_VERSION}" in
                        12.04) sudo /etc/init.d/php-fastcgi $1 ;;
                        14.04) sudo service php5-fpm $1 ;;
                    esac
            esac
    esac
}
function fastcgi-restart {
    sudo killall php-fpm php-fastcgi php-fastcgid $1; wait
    case "${OSTYPE}" in
        darwin*) ;;
        linux*)
            php-fastcgid start
            php-fastcgid status ;;
    esac
}
alias fr="fastcgi-restart"
alias fp="ps aux | \grep -G 'php.*'"
alias fs="php-fastcgid status"
alias fk="php-fastcgid stop"


# 2. ProgrammingLanguage::Python
# ------------------------------
REQUIRED_PYTHON_VERSION=2.7.11
export PATH="$HOME/.parts/packages/python2/$REQUIRED_PYTHON_VERSION/bin:$PATH"
# ### version control ###
function get-pyenv {
    case "${OSTYPE}" in
        freebsd*|darwin*|linux*) anyenv install pyenv && exec -l zsh ;;
    esac
}
if ! type -p pyenv > /dev/null ; then get-pyenv ; fi
# ### installation ###
function get-python {
    case "${OSTYPE}" in
        cygwin)
            apt-cyg install \
                    python
            python-setuptools ;;
        freebsd*|darwin*|linux*)
            pyenv install $REQUIRED_PYTHON_VERSION
            pyenv rehash
            pyenv global $REQUIRED_PYTHON_VERSION
    esac
}
if ! type -p python > /dev/null; then get-python ; fi
function get-pip {
    case "${OSTYPE}" in
        cygwin)
            if type -p easy_install-2.7 > /dev/null ; then
                easy_install-2.7 https://pypi.python.org/packages/source/p/pip/pip-1.4.1.tar.gz
                get-global-pip-packages
            elif type -p easy_install-2.7 > /dev/null ; then
                easy_install https://pypi.python.org/packages/source/p/pip/pip-1.4.1.tar.gz
                get-global-pip-packages
            fi ;;
        freebsd*|darwin*|linux*)
            easy_install pip
            get-global-pip-packages ;;
    esac
}
function get-global-pip-packages {
    case "$OSTYPE" in
        freebsd*|darwin*|linux*)
            pip install -U \
                docker-compose \
                ipython \
                pandas \
                pulp \
                simpy \
                boto ;;
    esac
}
if ! type -p pip > /dev/null ; then get-pip ; fi


# 2. ProgrammingLanguage::Perl
# ----------------------------
REQUIRED_PERL_VERSION=5.20.3
export PATH="$HOME/.cask/bin:$PATH"
# ### version control ###
function get-plenv {
    case "${OSTYPE}" in
        freebsd*|darwin*|linux*) anyenv install plenv && exec -l zsh ;;
    esac
}
if ! type -p plenv > /dev/null ; then get-plenv ; fi
# ### installation ###
function get-perl {
    case "${OSTYPE}" in
        cygwin) apt-cyg install perl ;;
        freebsd*|darwin*)
            plenv install $REQUIRED_PERL_VERSION
            plenv rehash
            plenv global $REQUIRED_PERL_VERSION
            plenv install-cpanm ;;
        linux*)
            case $DIST in
                Redhat|RedHat)
                    nix-install perl-$REQUIRED_PERL_VERSION
                    nix-install perl-App-cpanminus ;;
                Debian|Ubuntu)
                    plenv install $REQUIRED_PERL_VERSION
                    plenv rehash
                    plenv global $REQUIRED_PERL_VERSION
                    plenv install-cpanm ;;
            esac
    esac
}
if ! type -p perl > /dev/null ; then get-perl ; fi
# eval $(perl -I$HOME/.local/lib/perl5 -Mlocal::lib=$HOME/.local)
# ### plagger ###
function get-plagger {
    case "${OSTYPE}" in
        cygwin) ;;
        freebsd*|darwin*)
            cpanm -fi YAML::Loader \
                  XML::LibXML \
                  XML::LibXML::SAX \
                  XML::LibXML::XPathContext \
                  XML::Liberal \
                  Text::Glob \
                  Module::Runtime \
                  Params::Util \
                  Digest::SHA1 \
                  Class::Load \
                  XML::RSS \
                  XML::RSS::LibXML \
                  XML::RSS::Liberal \
                  XML::Feed \
                  XML::Feed::RSS \
                  XML::Atom \
                  WebService::Bloglines \
                  Plagger ;;
        linux*)
            case $DIST in
                Redhat|RedHat) ;;
                Debian|Ubuntu)
                    cpanm -fi YAML::Loader \
                          XML::LibXML \
                          XML::LibXML::SAX \
                          XML::LibXML::XPathContext \
                          XML::Liberal \
                          Text::Glob \
                          Module::Runtime \
                          Params::Util \
                          Digest::SHA1 \
                          Class::Load \
                          XML::RSS \
                          XML::RSS::LibXML \
                          XML::RSS::Liberal \
                          XML::Feed \
                          XML::Feed::RSS \
                          XML::Atom \
                          WebService::Bloglines \
                          Plagger
            esac
    esac
}
# ### org-asana ###
function get-org-asana {
    yes | cpanm -fi Moose \
                WWW::Asana \
                Org::Parser \
                YAML
}
function get-global-cpan-packages {
    yes | cpanm -fi Carton
}
# ### cpan ###
function cpan-module-list { perl -e "print \"@INC\"" | find -name "*.pm" -print }
function cpan-module-version { perl -M$1 -le "print \$$1::VERSION" }
function cpan-uninstall { perl -MConfig -MExtUtils::Install -e '($FULLEXT=shift)=~s{-}{/}g;uninstall "$Config{sitearchexp}/auto/$FULLEXT/.packlist",1' }
alias cpanmini='cpan --mirror ~/.cpan/minicpan --mirror-only'
# alias cpan-uninstall='perl -MConfig -MExtUtils::Install -e '"'"'($FULLEXT=shift)=~s{-}{/}g;uninstall "$Config{sitearchexp}/auto/$FULLEXT/.packlist",1'"'"


# 2. ProgrammingLanguage::Javascript
# ----------------------------------
export REQUIRED_NODE_VERSION='5.8.0'
export node='NODE_NO_READLINE=1 node'
case $OSTYPE in
    msys|cygwin)
        export PATH=$(get-winpath "C:\Program Files\nodejs"):$PATH ;;
esac
# ### version control ###
function get-ndenv {
    case "${OSTYPE}" in
        freebsd*|darwin*|linux*) anyenv install ndenv && exec -l zsh ;;
    esac
}
if ! type -p ndenv > /dev/null ; then get-ndenv ; fi
# ### installation ###
function get-node {
    case "$OSTYPE" in
        msys|cygwin) choco install nodejs ;;
        linux*)
            ndenv install v$REQUIRED_NODE_VERSION
            ndenv rehash
            ndenv global v$REQUIRED_NODE_VERSION
            get-global-npm-packages ;;
    esac
}
function set-node {
    case "$OSTYPE" in
        linux*) ndenv global v$REQUIRED_NODE_VERSION
    esac
}
function get-global-npm-packages {
    npm install -g \
        bower \
        grunt-cli \
        gulp \
        html2jade \
        http-server \
        less \
        node-plantuml \
        npm2dot \
        phantomjs \
        requirejs \
        yamljs \
        tern
}
if ! type -p npm > /dev/null ; then get-node ; fi
if type -p npm > /dev/null ; then set-node ; fi
function rebuild-sass {
    npm uninstall --save-dev gulp-sass
    npm install --save-dev gulp-sass@2
    npm rebuild node-sass
}


# 2. ProgrammingLanguage::RemoteProcedureCall
# -------------------------------------------
function get-protobuf {
    case "${OSTYPE}" in
        freebsd*|darwin*) nix-install protobuf-2.6.1 ;;
        linux*)
            case $DIST_VERSION in
                14.04) ;;
                *) nix-install protobuf-2.6.1 ;;
            esac
    esac
}
function get-thrift {
    case "${OSTYPE}" in
        freebsd*|darwin*) nix-install thrift-0.9.3 ;;
        linux*)
            case $DIST_VERSION in
                14.04) ;;
                *) nix-install thrift-0.9.3 ;;
            esac
    esac
}
if ! type -p protoc > /dev/null ; then get-protobuf ; fi
if ! type -p thrift > /dev/null ; then get-thrift ; fi


# 3. Daemon::Database::Postgresql
# -------------------------------
REQUIRED_POSTGRESQL_VERSION=9.5.3
PSQL_PAGER='less -S'
# ### installation ###
function get-postgresql {
    case "${OSTYPE}" in
        darwin*|linux*)
            docker run \
                   --name postgres-$REQUIRED_POSTGRESQL_VERSION \
                   -p 5432:5432 \
                   -e POSTGRES_PASSWORD=password \
                   -d \
                   postgres:$REQUIRED_POSTGRESQL_VERSION ;;
    esac
}
function postgresql-restart {
    case "${OSTYPE}" in
        darwin*|linux*) docker restart postgres-$REQUIRED_POSTGRESQL_VERSION ;;
    esac
}
function postgresql-stop {
    case "${OSTYPE}" in
        darwin*|linux*) docker stop postgres-$REQUIRED_POSTGRESQL_VERSION ;;
    esac
}
alias pgr="postgresql-restart"
alias pgp="dp"
alias pgs="dp"
alias pgk="postgresql-stop"


# 3. Daemon::Database::Mysql
# --------------------------
REQUIRED_MYSQL_VERSION_1=5.6
REQUIRED_MYSQL_VERSION_2=5.7
REQUIRED_MYSQL_PORT_1=3306
REQUIRED_MYSQL_PORT_2=3307
REQUIRED_MYSQL_PASSWORD_1=password
REQUIRED_MYSQL_PASSWORD_2=password
# ### installation ###
function set-mycnf {
    mkdir ~/.mysql
    local my_cnf='
[mysqld]

# -----------------------------------------------------
# Base
# -----------------------------------------------------
character-set-server = utf8mb4
skip-character-set-client-handshake
skip-external-locking
sql_mode             = NO_ENGINE_SUBSTITUTION,STRICT_TRANS_TABLES

# -----------------------------------------------------
# Network
# -----------------------------------------------------
max_connections     = 31
max_connect_errors  = 1000000
wait_timeout        = 600
interactive_timeout = 600
connect_timeout     = 10
max_allowed_packet  = 1GB

# -----------------------------------------------------
# Logging
# -----------------------------------------------------
log_output                    = FILE
log_error                     = /var/log/mysql/error.log
general_log_file              = /var/log/mysql/general_query.log
log-slow-admin-statements     = 1
log-queries-not-using-indexes = 1
slow_query_log                = 1
long_query_time               = 1
slow_query_log_file           = /var/log/mysql/slow_query.log

# # -----------------------------------------------------
# # Cache & Memory
# # -----------------------------------------------------
# thread_cache_size   = 31
# table_open_cache    = 400
# max_heap_table_size = 33554432
# max_heap_table_size = 33554432

# # -----------------------------------------------------
# # InnoDB
# # -----------------------------------------------------
# innodb_file_format              = Barracuda
# innodb_file_per_table           = 1
# innodb_large_prefix
# innodb_buffer_pool_size         = 1GB
# innodb_additional_mem_pool_size = 20MB
# innodb_log_buffer_size          = 64MB

[mysqld_safe]
log-error = /var/log/mysqld.log
pid-file  = /var/run/mysqld/mysqld.pid
socket    = /var/run/mysqld/mysqld.sock
nice      = 0

[client]
default-character-set = utf8mb4

[mysqldump]
quick
quote-names
max_allowed_packet = 32M
default-character-set = utf8mb4

[mysql]
default-character-set = utf8mb4
prompt = "\u@\h\_[\d]\_\R:\m:\\s>\_"
pager = "less -n -i -F -X -E"
'
    echo $my_cnf | tee --append ~/.mysql/my.cnf
}
if [ ! -f ~/.mysql/my.cnf ]; then set-mycnf; fi
function get-mysql {
    case $OSTYPE in
        darwin*|linux*)
            docker run \
                   --name mysql-$REQUIRED_MYSQL_VERSION_1 \
                   -v $HOME/.mysql:/etc/mysql/conf.d \
                   -p $REQUIRED_MYSQL_PORT_1:3306 \
                   -e MYSQL_ROOT_PASSWORD=$REQUIRED_MYSQL_PASSWORD_1 \
                   -d \
                   mysql:$REQUIRED_MYSQL_VERSION_1
            docker run \
                   --name mysql-$REQUIRED_MYSQL_VERSION_2 \
                   -v $HOME/.mysql:/etc/mysql/conf.d \
                   -p $REQUIRED_MYSQL_PORT_2:3306 \
                   -e MYSQL_ROOT_PASSWORD=$REQUIRED_MYSQL_PASSWORD_2 \
                   -d \
                   mysql:$REQUIRED_MYSQL_VERSION_2
    esac
}
function mysql-restart {
    case "${OSTYPE}" in
        darwin*|linux*)
            docker restart mysql-$REQUIRED_MYSQL_VERSION_1
            docker restart mysql-$REQUIRED_MYSQL_VERSION_2
    esac
}
function mysql-stop {
    case "${OSTYPE}" in
        darwin*|linux*)
            docker stop mysql-$REQUIRED_MYSQL_VERSION_1
            docker stop mysql-$REQUIRED_MYSQL_VERSION_2
    esac
}
# ### innotop ###
function get-innotop {
    yes | cpanm -fi DBI \
                DBD::mysql \
                ExtUtils::MakeMaker \
                Time::HiRes \
                TermReadKey
    git clone https://github.com/innotop/innotop
    cd innotop
    perl Makefile.PL
    make install
    cd ..
    rm -fr innotop
}
alias mr="mysql-restart"
alias mp="dp"
alias ms="dp"
alias mk="mysql-stop"


# 3. Daemon::Database::Redis
# --------------------------
REQUIRED_REDIS_VERSION=2.8
REQUIRED_REDIS_PORT=6379
# ### installation ###
function get-redis {
    case "${OSTYPE}" in
        darwin*|linux*)
            docker run \
                   --name redis-$REQUIRED_REDIS_VERSION \
                   -p $REQUIRED_REDIS_PORT:6379 \
                   -d \
                   redis:$REQUIRED_REDIS_VERSION
    esac
}
function redis-restart {
    case "${OSTYPE}" in
        darwin*|linux*) docker restart redis-$REQUIRED_REDIS_VERSION ;;
    esac
}
function redis-stop {
    case "${OSTYPE}" in
        darwin*|linux*) docker stop redis-$REQUIRED_REDIS_VERSION ;;
    esac
}
function redis-cli {
    case "${OSTYPE}" in
        darwin*|linux*)
            docker run \
                   -it \
                   --link redis-${REQUIRED_REDIS_VERSION}:redis \
                   --rm \
                   redis \
                   redis-cli -h redis -p $REQUIRED_REDIS_PORT $1
    esac
}
alias redis-cli-monitor='redis-cli monitor'
alias redis-cli-info='redis-cli info'
alias redis-cli-dump='redis-cli bgsave'
alias redis-cli-top='redis-cli --stat'
alias rdr="redis-restart"
alias rdp="dp"
alias rds="dp"
alias rdi="redis-cli-info"
alias rdm="redis-cli-monitor"
alias rdt="redis-cli-top"
alias rdk="redis-stop"


# 3. Daemon::Database::Memcached
# ------------------------------
REQUIRED_MEMCACHED_VERSION=1.4
REQUIRED_MEMCACHED_PORT=11211
function get-memcached {
    case "${OSTYPE}" in
        darwin*|linux*)
            docker run \
                   --name memcached-$REQUIRED_MEMCACHED_VERSION \
                   -p $REQUIRED_MEMCACHED_PORT:11211 \
                   -d \
                   memcached:$REQUIRED_MEMCACHED_VERSION
    esac
}
function get-memcached-tool {
    wget https://raw.githubusercontent.com/memcached/memcached/master/scripts/memcached-tool -O ~/.local/bin/memcached-tool
    chmod +x ~/.local/bin/memcached-tool
}
if ! type -p memcached-tool > /dev/null ; then get-memcached-tool ; fi
function memcached-restart {
    case "${OSTYPE}" in
        darwin*|linux*) docker restart memcached-$REQUIRED_MEMCACHED_VERSION ;;
    esac
}
function memcached-stop {
    case "${OSTYPE}" in
        darwin*|linux*) docker stop memcached-$REQUIRED_MEMCACHED_VERSION ;;
    esac
}
function memcached-tool {
    case "${OSTYPE}" in
        darwin*|linux*)
            docker run \
                   -it \
                   --link memcached-${REQUIRED_MEMCACHED_VERSION}:memcached \
                   --rm \
                   memcached \
                   memcached-tool 127.0.0.1:$REQUIRED_MEMCACHED_PORT $1
    esac
}
alias memcached-monitor='memcached-tool 127.0.0.1:11211 display'
alias memcached-info='memcached-tool 127.0.0.1:11211 stats'
alias memcached-dump='memcached-tool 127.0.0.1:11211 dump'
alias mcr="memcached-restart"
alias mcp="dp"
alias mci="memcached-info"
alias mcm="memcached-monitor"
alias mck="memcached-stop"


# 3. Daemon::HttpServer::Nginx
# ----------------------------
REQUIRED_NGINX_VERSION=1.10
REQUIRED_NGINX_HTTP_PORT=80
REQUIRED_NGINX_HTTPS_PORT=443
function get-nginx {
    case "${OSTYPE}" in
        darwin*|linux*)
            docker run \
                   --name nginx-$REQUIRED_NGINX_VERSION \
                   -p $REQUIRED_NGINX_HTTP_PORT:80 \
                   -p $REQUIRED_NGINX_HTTPS_PORT:443 \
                   -d \
                   nginx:$REQUIRED_NGINX_VERSION
            docker cp nginx-$REQUIRED_NGINX_VERSION:/etc/nginx/nginx.conf $HOME/.nginx/nginx.conf
            docker cp nginx-$REQUIRED_NGINX_VERSION:/etc/nginx/conf.d $HOME/.nginx/conf.d
    esac
}
function nginx-restart {
    case "${OSTYPE}" in
        darwin*|linux*) docker restart nginx-$REQUIRED_NGINX_VERSION
    esac
}
function nginx-stop {
    case "${OSTYPE}" in
        darwin*|linux*) docker stop nginx-$REQUIRED_NGINX_VERSION
    esac
}
alias nr="nginx-restart"
alias np="dp"
alias ns="dp"
alias nk="nginx-stop"


# 4. IntegratedDevelopmentEnvironment::Emacs
# ------------------------------------------
export REQUIRED_EMACS_VERSION=24.5
# ### installation ###
function get-emacs {
    case "${OSTYPE}" in
        msys) pacman -S mingw-w64-x86_64-emacs ;;
        cygwin) apt-cyg install emacs ;;
        freebsd*|darwin*|linux*)
            case "${DIST}" in
                Redhat|RedHat)
                    sudo yum install -y ncurses-devel
                    local current_pwd=`pwd`
                    wget https://ftp.gnu.org/gnu/emacs/emacs-$REQUIRED_EMACS_VERSION.tar.gz;  wait
                    tar zxf emacs-$REQUIRED_EMACS_VERSION.tar.gz;  wait
                    cd emacs-$REQUIRED_EMACS_VERSION
                    ./configure --with-xpm=no --with-gif=no --with-x-toolkit=no --with-tiff=no
                    make
                    yes | sudo make install;  wait
                    cd $current_pwd; rm -fr emacs-$REQUIRED_EMACS_VERSION* ;;
                Debian|Ubuntu)
                    sudo add-apt-repository ppa:ubuntu-elisp/ppa
                    sudo apt-get update
                    sudo apt-get install emacs-snapshot ;;
            esac
    esac
}
case $OSTYPE in (msys) alias emacs='/mingw64/bin/emacs -nw' ;; esac
if ! type emacs > /dev/null; then
    get-emacs
else
    _CURRENT_EMACS_VERSION=$(emacs --version | head -n 1 | sed 's/GNU Emacs //' | awk '$0 = substr($0, 1, index($0, ".") + 1)')
    if [[ $_REQUIRED_EMACS_VERSION > $_CURRENT_EMACS_VERSION ]]; then get-emacs; fi
fi
# ### aspell ###
function get-aspell {
    case "${OSTYPE}" in
        darwin*) brew install --with-lang-ena aspell ;;
        freebsd*) ;;
        linux*)
            case "${DIST}" in
                Debian|Ubuntu) sudo apt-get install -y aspell ;;
            esac
    esac
}
if ! type -p aspell > /dev/null ; then get-aspell ; fi
# ### mu4e ###
function get-mu {
    case "${OSTYPE}" in
        freebsd*|darwin*|linux*)
            case "${DIST}" in
                Debian) ;;
                Ubuntu)
                    case $DIST_VERSION in
                        14.04)
                            sudo apt-get install -y \
                                 libgmime-2.6-dev \
                                 libxapian-dev \
                                 gnutls-bin \
                                 guile-2.0-dev \
                                 html2text \
                                 xdg-utils \
                                 offlineimap
                            \git clone https://github.com/djcb/mu
                            cd mu
                            sudo autoreconf -i
                            ./configure && make
                            sudo make install
                            cd .. && sudo rm -fr mu
                            ln -s /usr/local/share/emacs/site-lisp/mu4e $HOME/.emacs.d/site-lisp/ ;;
                    esac
            esac
    esac
}
function mu-restart {
    mv ~/Maildir ~/Maildir.org$(date +%y%m%d)
    mkdir -p ~/Maildir
    rm -fr ~/.mu
    rm -fr ~/.offlineimap
    offlineimap
    mu index --maildir=~/Maildir
    mu index --rebuild
    mu index
}
### if ! type -p mu > /dev/null ; then get-mu ; fi
# ### sekka ###
function get-sekka {
    case "${OSTYPE}" in
        freebsd*|linux*)
            docker run \
                   --name=sekka \
                   -p 12929:12929 \
                   -d -t \
                   kiyoka/sekka ;;
    esac
}
function sekka-restart {
    case "${OSTYPE}" in
        freebsd*|linux*) docker restart sekka ;;
    esac
}


# 4. IntegratedDevelopmentEnvironment::Emacs::Ctags
# -------------------------------------------------
REQUIRED_CTAGS_VERSION=5.8
function get-ctags {
    case "${OSTYPE}" in
        freebsd*linux*)
            local current_pwd=`pwd`
            wget http://prdownloads.sourceforge.net/ctags/ctags-$REQUIRED_CTAGS_VERSION.tar.gz
            tar zxf ctags-$REQUIRED_CTAGS_VERSION.tar.gz
            cd ctags-$REQUIRED_CTAGS_VERSION
            ./configure --prefix=$HOME/.local
            make
            sudo make install
            cd $current_pwd ;;
    esac
}
if ! type -p ctags > /dev/null ; then get-ctags ; fi
alias ctags=~/.local/bin/ctags


# 4. IntegratedDevelopmentEnvironment::Emacs::Pandoc
# --------------------------------------------------
REQUIRED_PANDOC_VERSION=1.17.0.3
function get-pandoc {
    case $OSTYPE in
        freebsd*|darwin*) nix-install pandoc-${REQUIRED_PANDOC_VERSION} ;;
        linux*)
            case $DIST_VERSION in
                14.04) ;;
                *) nix-install pandoc-${REQUIRED_PANDOC_VERSION} ;;
            esac
    esac
}
if ! type -p pandoc > /dev/null ; then get-pandoc ; fi
alias pandocpdf="pandoc -V documentclass=ltjarticle --latex-engine=lualatex -t pdf"
alias pandocslide="pandoc -t slidy -s"


# 4. IntegratedDevelopmentEnvironment::ResourceManagement::Filesystem
# -------------------------------------------------------------------
# ### inotify ###
function get-inotify {
    case "${OSTYPE}" in
        freebsd*|darwin*) ;;
        linux*)
            case "${DIST}" in
                Redhat|RedHat) sudo yum install -y inotify-tools ;;
                Debian|Ubuntu) sudo apt-get install -y inotify-tools ;;
            esac
    esac
}
if ! type -p inotifywait > /dev/null ; then get-inotify ; fi
# ### storage minimization ###
function delete-log {
    sudo find /home /var /usr -mtime +1 -a \( -name "*.pag" -o -name "*.dir" -o -name "*.log" \) -exec sudo rm {} \;
}
function sync-filesystem {
    delete-log
    crontab -r
    echo '0 0 * * * sudo find /home /var /usr -mtime +1 -a \( -name "*.pag" -o -name "*.dir" -o -name "*.log" \) -exec sudo rm {} \;' | crontab
}


# 4. IntegratedDevelopmentEnvironment::ResourceManagement::Git
# ------------------------------------------------------------
REQUIRED_GIT_VERSION=1.9.4
# ### installation ###
function get-git {
    case "${OSTYPE}" in
        freebsd*|darwin*) get-git-flow ;;
        linux*)
            case "${DIST}" in
                Redhat|RedHat)
                    sudo yum install -y \
                         perl-ExtUtils-MakeMaker \
                         libcurl-devel
                    wget https://www.kernel.org/pub/software/scm/git/git-${REQUIRED_GIT_VERSION}.tar.gz
                    tar zxvf git-${REQUIRED_GIT_VERSION}.tar.gz
                    cd git-${REQUIRED_GIT_VERSION}
                    ./configure --prefix=/usr/local --with-curl --with-expat
                    make prefix=/usr/local all
                    sudo make prefix=/usr/local install
                    cd ..
                    rm -fr git-${REQUIRED_GIT_VERSION} git-${REQUIRED_GIT_VERSION}.tar.gz
                    get-git-flow ;;
                Debian) ;;
                Ubuntu)
                    case "${DIST_VERSION=}" in
                        12.04) sudo apt-get install -y python-software-properties ;;
                        14.04) sudo apt-get install -y software-properties-common python3-software-properties ;;
                    esac
                    sudo add-apt-repository ppa:git-core/ppa
                    sudo apt-get update -y
                    sudo apt-get install -y git
                    get-git-flow ;;
            esac
            git config --global push.default simple
    esac
}
function get-git-flow {
    case "${OSTYPE}" in
        freebsd*|darwin*) port install git-flow ;;
        linux*)
            case "${DIST}" in
                Redhat|RedHat) sudo yum install -y gitflow ;;
                Debian|Ubuntu) sudo apt-get install -y git-flow ;;
            esac
    esac
}
if ! type -p git > /dev/null; then
    get-git
else
    _REQUIRED_GIT_VERSION_NUM=$(echo $REQUIRED_GIT_VERSION | sed 's/\(.*\..*\)\..*/\1/')
    _CURRENT_GIT_VERSION=$(git --version 2>&1 | cut -d\  -f 3 | sed 's/\(.*\..*\)\..*/\1/')
    if [[ $_REQUIRED_GIT_VERSION_NUM > $_CURRENT_GIT_VERSION ]]; then get-git; fi
fi
# ### hub ###
function get-hub {
    case "${OSTYPE}" in
        freebsd*) ;;
        darwin*) brew install hub ;;
        linux*)
            case "${DIST}" in
                Redhat|RedHat) sudo yum install -y hub ;;
                Debian) ;;
                Ubuntu)
                    case "$DIST_VERSION" in
                        12.04) parts install hub ;;
                        14.04) ;;
                    esac
            esac
    esac
}
if ! type -p hub > /dev/null ; then get-hub ; fi
if type -p hub > /dev/null ; then eval "$(hub alias -s)" ; fi
# ### gibo ###
function get-gibo {
    case "${OSTYPE}" in
        freebsd*) ;;
        darwin*|linux*)
            brew install gibo ;;
    esac
}
if ! type -p gibo > /dev/null ; then get-gibo ; fi
# ### bfg ###
function get-bfg {
    REQUIRED_BFG_VERSION=1.12.12
    case "${OSTYPE}" in
        freebsd*|darwin*|linux*)
            wget http://repo1.maven.org/maven2/com/madgag/bfg/${REQUIRED_BFG_VERSION}/bfg-${REQUIRED_BFG_VERSION}.jar  -O ~/.local/bin/bfg.jar
            alias bfg='java -jar ~/.local/bin/bfg.jar' ;;
    esac
}
if [ ! -f ~/.local/bin/bfg.jar ] ; then get-bfg ; fi
if [ -f ~/.local/bin/bfg.jar ] ; then alias bfg='java -jar ~/.local/bin/bfg.jar' ; fi
# ### alias ###
alias g='git'
alias ga='git add -v'
alias galiases="git !git config --get-regexp 'alias.*' | colrm 1 6 | sed 's/[ ]/ = /'"
alias gammend='git commit --amend'
alias gb='git branch'
alias gbr='git branch'
alias gbranches='git branch -a'
alias gbrs='git branch -a'
alias gc='git commit -a -v -m'
alias gca='git commit -a -v -m'
alias gcd="git !bash -c 'while  ! -d .git; do cd ..; done'"
alias gci='git commit -v'
alias gco='git checkout'
alias gd='git rm'
alias gdel='git rm'
alias gdelbr='git !git branch -D'
alias gdb='git !git branch -D'
alias gdrb='git "!sh -c \"git push origin --delete $1\""'
alias gdf='git diff HEAD^'
alias gdfc='git diff --cached'
alias gexport='git "!sh -c \"git checkout-index -a -f --prefix=$1/\" -"'
alias ghist='git log --color --graph --pretty=format:"%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset" --abbrev-commit --'
function ghclone { git clone git@github.com:${1}.git }
function git-log { \git log ${1} --color --graph --pretty=format:"%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset" --abbrev-commit -- }
alias gl='git-log'
alias glast='git diff HEAD~1..HEAD'
alias glf='git log --decorate=full --graph --pretty=full'
alias glg='git log --color --graph --pretty=format:"%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset" --abbrev-commit --'
alias gpick='git cherry-pick'
alias gp='git push -v'
alias gpom='git push -v -u origin master'
alias gpr='git pull --rebase'
alias gs='git status -sb'
alias gsearch=git "!sh -c \'git rev-list --all | grep ^$1 | while read commit; do git --no-pager log -n1 --pretty=format:\"%H %ci %an %s%n\" $commit; done\' -"
alias gslog='git log --graph --pretty=format:"%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset" --abbrev-commit --date=relative'
alias gst='git status -sb'
alias gswitch='git checkout'
alias gvlog='git log --graph --pretty=format:"%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen - %cD (%cr) %C(bold blue)<%an>%Creset%n" --abbrev-commit --date=relative -p ; echo ""'
alias gwho='git shortlog -s --'
function git-trend {
    g trend -n 10
    g trend -l ruby -n 5
    g trend -l elixir -n 5
    g trend -l erlang -n 5
    g trend -l JavaScript -n 5
    g trend -l php -n 3
}
function parse-git-dirty { git diff --no-ext-diff --quiet --exit-code &> /dev/null || echo "*" }
function parse-git-branch { git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e "s/* \(.*\)/(\1$(parse_git_dirty))/" }
function github-pull-repositories {
    # # Setup Example:
    # function setup-git-pull-repositories {
    #     case $1 in
    #         foo) echo 'npm install';;
    #         bar) echo 'npm install && bower install';;
    #     esac
    # }
    typeset -A users
    typeset -A branches
    typeset -A build_commands
    while getopts "n:r:" opt; do
        case $opt in
            n)
                dirname=$OPTARG
                is_dirname='true'
                ;;
            r)
                array=("${(@s:,:)OPTARG}")
                repository=$array[2]
                users[$repository]=$array[1]
                branches[$repository]=$array[3]
                build_commands[$repository]=`setup-github-pull-repositories $repository`
                is_repositoy='true'
                ;;
        esac
    done
    if [ $# -ge 4 ] && [ $is_dirname ] && [ $is_repositoy ]; then
        local directory=$(date +%y%m%d)_$dirname
        local current_pwd=`pwd`
        cd ~/
        mkdir -p ~/${directory}
        cd ~/${directory}
        for k in ${(@k)build_commands}; do
            echo ''
            echo 'Start git clone:' $k
            echo '--------------------------------------------------'
            git clone git@github.com:$users[$k]/$k.git; wait
            cd ./$k
            echo ''
            echo 'Start build:' $build_commands[$k] 'in' $k
            echo '--------------------------------------------------'
            git checkout $branches[$k]
            eval $build_commands[$k]; wait
            cd ~/${directory}
        done
        cd ${current_pwd}
        echo ''
        echo ''
        echo '   _______ __  ____        ____'
        echo '  / ____(_) /_/ __ \__  __/ / /'
        echo ' / / __/ / __/ /_/ / / / / / /'
        echo '/ /_/ / / /_/ ____/ /_/ / / /'
        echo '\____/_/\__/_/    \__,_/_/_/'
        echo '                                ....is now installed!'
    else
        echo ''
        echo "Usage: github-pull-repositories -r user1,repository1,branch1 -r user2,repository2,branch2 -n dirname .." 1>&2
    fi
}


# 4. IntegratedDevelopmentEnvironment::ResourceManagement::PlantUml
# -----------------------------------------------------------------
REQUIRED_GRAPHVIZ_VERSION=2.38
case $OSTYPE in
    msys)   export PATH=/C/Program~2/Graphviz${REQUIRED_GRAPHVIZ_VERSION}/bin:$PATH ;;
    cygwin) export PATH=/cygdrive/C/Program~2/Graphviz${REQUIRED_GRAPHVIZ_VERSION}/bin:$PATH ;;
esac
function get-puml {
    case "${OSTYPE}" in
        freebsd*|darwin*) ;;
        linux*) if type -p npm > /dev/null; then npm install -g node-plantuml; fi ;;
    esac
}
function get-plantuml {
    case "${OSTYPE}" in
        freebsd*|darwin*) ;;
        linux*)
            wget http://jaist.dl.sourceforge.net/project/plantuml/plantuml.8027.jar -O ~/.local/bin/plantuml.jar
            alias plantuml='java -jar ~/.local/bin/plantuml.jar -tpng' ;;
    esac
}
function get-graphviz {
    case "${OSTYPE}" in
        msys|cygwin) choco install graphviz ;;
        freebsd*|darwin*) brew install graphviz ;;
        linux*)
            case "${DIST}" in
                Redhat|RedHat) sudo yum install -y graphviz ;;
                Debian|Ubuntu)
                    sudo apt-get update -y
                    sudo apt-get install -y graphviz ;;
            esac
    esac
}
if ! type -p puml > /dev/null ; then get-puml ; fi
if [ ! -f ~/.local/bin/plantuml.jar ] ; then get-plantuml ; fi
if [ -f ~/.local/bin/plantuml.jar ] ; then alias plantuml='java -jar ~/.local/bin/plantuml.jar -tpng' ; fi
if ! type -p dot > /dev/null ; then get-graphviz ; fi


# 4. IntegratedDevelopmentEnvironment::SoftwareDebugging::Benchmark
# -----------------------------------------------------------------
function get-ab {
    case "${OSTYPE}" in
        freebsd*|darwin*) ;;
        linux*)
            case "${DIST}" in
                Redhat|RedHat) sudo yum install -y httpd-tools ;;
                Debian|Ubuntu) sudo apt-get install -y apache2-utils ;;
            esac
    esac
}
if ! type -p ab > /dev/null ; then get-ab ; fi
function get-wrk {
    case "${OSTYPE}" in
        freebsd*) ;;
        darwin*) brew install wrk ;;
        linux*)
            case "${DIST}" in
                Redhat|RedHat)
                    cd ~
                    sudo yum groupinstall 'Development Tools'
                    sudo yum install openssl-devel
                    git clone https://github.com/wg/wrk.git
                    cd wrk && make && cp wrk ~/.local/bin
                    cd ~ && rm -fr wrk ;;
                Debian|Ubuntu)
                    cd ~
                    sudo apt-get install build-essential libssl-dev
                    git clone https://github.com/wg/wrk.git
                    cd wrk && make && cp wrk ~/.local/bin
                    cd ~ && rm -fr wrk ;;
            esac
    esac
}
if ! type -p wrk > /dev/null ; then get-wrk ; fi


# 4. IntegratedDevelopmentEnvironment::OsLevelVirtualization::Vagrannt
# --------------------------------------------------------------------
alias vu='vagrant up'
alias vt='vagrant halt'
alias vr='vagrant reload'
alias vp='vagrant global-status'
alias vk='vagrant destroy --force'
alias vl='vagrant box list'
alias vd='vagrant box remove'
alias vsh='vagrant ssh'
alias vshconfig='vagrant ssh-config'
case $OSTYPE in
    msys|cygwin)
        export PATH=$(get-winpath 'C:\HashiCorp\Vagrant\bin'):$PATH
        export VBOX_MSI_INSTALL_PATH=$(get-winpath $VBOX_MSI_INSTALL_PATH) ;;
esac
# ### vagrant ###
function get-vagrant {
    case $OSTYPE in
        msys|cygwin) choco install vagrant ;;
        linux*)
            case $DIST in
                Debian|Ubuntu)
                    sudo bash -c 'echo deb http://vagrant-deb.linestarve.com/ any main > /etc/apt/sources.list.d/wolfgang42-vagrant.list'
                    sudo apt-key adv --keyserver pgp.mit.edu --recv-key AD319E0F7CFFA38B4D9F6E55CE3F3DE92099F7A4
                    sudo apt-get update
                    sudo apt-get install vagrant
            esac
    esac
}
if ! type -p vagrant > /dev/null ; then get-vagrant ; fi
# ### virtualbox ###
function vbm-scaleup {
    while getopts "i:s:" opt; do
        case $opt in
            i) image=$OPTARG ; is_image='true' ;;
            s) size=$OPTARG  ; is_size='true'  ;;
        esac
    done
    if [[ $# = 4 ]] && [ $is_image ] && [ $is_size ]; then
        current_basename=$(basename `pwd`)
        uuid=$(VBoxManage list vms | \grep $current_basename | cut -f 2 -d " " | sed -e 's/[\{\}]//g')
        VBoxManage clonehd $image.vmdk $image.vdi --format vdi; wait
        VBoxManage modifyhd $image.vdi --resize $size; wait
        VBoxManage storagectl $uuid --name SATA --remove
        VBoxManage storagectl $uuid --name SATA --add SATA
        VBoxManage storageattach $uuid --storagectl SATA --type hdd --medium $image.vdi --port 0
    else
        echo ''
        echo "Usage: vbm-scaleup -i image[.vdmi] -s size[MB]" 1>&2
    fi
}
alias vbm='VBoxManage'


# 4. IntegratedDevelopmentEnvironment::SoftwareDeployment::Terraform
# ------------------------------------------------------------------
REQUIRED_TERRAFORM_VERSION=0.6.6
function get-terraform {
    local current_pwd=`pwd`
    cd ~/.local/bin
    rm -fr terraform*
    case "${OSTYPE}" in
        darwin*)
            wget https://releases.hashicorp.com/terraform/${REQUIRED_TERRAFORM_VERSION}/terraform_${REQUIRED_TERRAFORM_VERSION}_darwin_amd64.zip
            unzip terraform_${REQUIRED_TERRAFORM_VERSION}_darwin_amd64.zip
            ;;
        freebsd*)
            wget https://releases.hashicorp.com/terraform/${REQUIRED_TERRAFORM_VERSION}/terraform_${REQUIRED_TERRAFORM_VERSION}_freebsd_amd64.zip
            unzip terraform_${REQUIRED_TERRAFORM_VERSION}_freebsd_amd64.zip
            ;;
        linux*)
            wget https://releases.hashicorp.com/terraform/${REQUIRED_TERRAFORM_VERSION}/terraform_${REQUIRED_TERRAFORM_VERSION}_linux_amd64.zip
            unzip terraform_${REQUIRED_TERRAFORM_VERSION}_linux_amd64.zip
            ;;
    esac
    cd $current_pwd
}
if ! type -p terraform > /dev/null; then get-terraform; fi
function terraform-remote-config {
    terraform remote config -backend=S3 -backend-config="bucket=tfstate.d" -backend-config="key=$1.tfstate"
    terraform remote push
}
alias tfrc="terraform-remote-config"
alias tfr="terraform remote"
alias tfs="terraform show"
alias tfp="terraform plan"
alias tfa="terraform apply"
alias tfd="terraform destroy"


# 4. IntegratedDevelopmentEnvironment::ComputerTerminal::Zsh
# ----------------------------------------------------------
autoload colors
colors
case ${UID} in
    0)
	PROMPT="%B%{${fg[red]}%}%/#%{${reset_color}%}%b "
	PROMPT2="%B%{${fg[red]}%}%_#%{${reset_color}%}%b "
	SPROMPT="%B%{${fg[red]}%}%r is correct? [n,y,a,e]:%{${reset_color}%}%b "
	[ -n "${REMOTEHOST}${SSH_CONNECTION}" ] &&
            PROMPT="%{${fg[cyan]}%}$(echo ${HOST%%.*} | tr '[a-z]' '[A-Z]') ${PROMPT}" ;;
    *)
	PROMPT="%{${fg[red]}%}%/%%%{${reset_color}%} "
	PROMPT2="%{${fg[red]}%}%_%%%{${reset_color}%} "
	SPROMPT="%{${fg[red]}%}%r is correct? [n,y,a,e]:%{${reset_color}%} "
	[ -n "${REMOTEHOST}${SSH_CONNECTION}" ] &&
            PROMPT="%{${fg[cyan]}%}$(echo ${HOST%%.*} | tr '[a-z]' '[A-Z]') ${PROMPT}" ;;
esac
setopt auto_cd           # auto change directory
setopt auto_pushd        # auto directory pushd that you can get dirs list by cd -[tab]
setopt correct           # command correct edition before each completion attempt
setopt list_packed       # compacked complete list display
setopt noautoremoveslash # no remove postfix slash of command line
setopt nolistbeep        # no beep sound when complete list displayed


# 4. IntegratedDevelopmentEnvironment::ComputerTerminal::Zsh::Keybind
# -------------------------------------------------------------------
# ### emacs like keybind (e.x. Ctrl-a goes to head of a line and Ctrl-e goes to end of it) ###
bindkey -e
setopt  rm_star_silent
setopt extended_glob
# ### historical backward/forward search with linehead string binded to ^P/^N ###
autoload history-search-end
zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end history-search-end
bindkey "^p" history-beginning-search-backward-end
bindkey "^n" history-beginning-search-forward-end
bindkey "\\ep" history-beginning-search-backward-end
bindkey "\\en" history-beginning-search-forward-end
# ### Command history configuration ###
HISTFILE=~/.zsh_history
HISTSIZE=10000
SAVEHIST=10000
setopt hist_ignore_dups     # ignore duplication command history list
setopt share_history        # share command history data
# ### Completion configuration ###
# fpath=(~/.zsh/functions/Completion ${fpath})
# autoload -U compinit
# compinit -u
# ### editor ###
# autoload zed
# ### zmv ###
autoload -Uz zmv
# ### prediction ###
# autoload predict-on
# predict-off


# 4. IntegratedDevelopmentEnvironment::ComputerTerminal::Zsh::Terminal
# --------------------------------------------------------------------
unset LSCOLORS
unsetopt PROMPT_SP
zstyle ':completion:*' use-cache true
case "${TERM}" in
    xterm|screen.xterm) export TERM=xterm-color ;;
    kterm)
	export TERM=kterm-color
        # set BackSpace control character
	stty erase ;;
    cons25)
	unset LANG
	export LSCOLORS=ExFxCxdxBxegedabagacad
	export LS_COLORS='di=01;34:ln=01;35:so=01;32:ex=01;31:bd=46;34:cd=43;34:su=41;30:sg=46;30:tw=42;30:ow=43;30'
	zstyle ':completion:*' list-colors 'di=;34;1' 'ln=;35;1' 'so=;32;1' 'ex=31;1' 'bd=46;34' 'cd=43;34' ;;
    indumb|emacs)
	# PROMPT="%n@%~%(!.#.$)"
	PROMPT="%{${fg[red]}%}%/%%%{${reset_color}%} "
        PROMPT="%{${fg[cyan]}%}$(echo ${HOST%%.*} | tr '[a-z]' '[A-Z]') ${PROMPT}"
	RPROMPT=""
	unsetopt zle ;;
esac
# ### set terminal title including current directory ###
case "${TERM}" in
    kterm*|xterm*)
	function precmd {
	    echo -ne "\033]0;${USER}@${HOST%%.*}:${PWD}\007"
	}
	export LSCOLORS=gxfxcxdxbxegedabagacad
	export LS_COLORS='di=36;40:ln=35;40:so=32;40:pi=33;40:ex=31;40:bd=34;46:cd=34;43:su=0;41:sg=0;46:tw=0;42:ow=0;43:'
	zstyle ':completion:*' list-colors 'di=36' 'ln=35' 'so=32' 'ex=31' 'bd=46;34' 'cd=43;34' ;;
esac


# 4. IntegratedDevelopmentEnvironment::ComputerTerminal::Zsh::Alias
# -----------------------------------------------------------------
zmodload -i zsh/mathfunc
setopt complete_aliases     # aliased ls needs if file/dir completions work
# ### extentions ###
alias -s C=e
alias -s avi=svlc
alias -s bmp=display
alias -s bz2=bzcat
alias -s c=e
alias -s csv=ex
alias -s cpp=e
alias -s gif=display
alias -s gz=gzcat
alias -s h=e
alias -s html=w
alias -s java=e
alias -s jpeg=display
alias -s jpg=display
alias -s m4a=amarok
alias -s mp3=amarok
alias -s mp4v=svlc
alias -s mpeg=svlc
alias -s mpg=svlc
alias -s ogg=amarok
alias -s png=Fireworks
alias -s sh=e
alias -s tbz=bzcat
alias -s tgz=gzcat
alias -s txt=e
alias -s xhtml=w
alias -s xls=ex
alias -s xml=e
alias -s zip=zipinfo
# ### other aliases ###
alias today='date +%Y-%m-%d'
alias where="command -v"
case "${OSTYPE}" in
    freebsd*|darwin*)
	alias ls="ls -G -w"
        alias lf="\ls -p -l -F -G"
        alias ll="\ls -p -F -a -G"
        alias la="\ls -p -l -F -a -G" ;;
    linux*|cygwin|msys)
        alias ls='ls --color=auto'
        alias la="\ls -p -l -F -a --color=auto"
        alias lf="\ls -p -l -F --hide='.*' --color=auto"
        alias ll="\ls -p -F -a --color=auto"
        function lt {
            \ls -R $1 | \grep ":$" | \sed -e 's/:$//' -e 's/[^-][^\/]*\//--/g' -e 's/^/   /' -e 's/-/|/'
        }
esac
case "${OSTYPE}" in
    darwin*)
	alias updateports="sudo port selfupdate; sudo port outdated"
	alias portupgrade="sudo port upgrade installed" ;;
    freebsd*)
	case ${UID} in
	    0)
		updateports() {
		    if [ -f /usr/ports/.portsnap.INDEX ] ; then
			portsnap fetch update
		    else
			portsnap fetch extract update
		    fi
		    (cd /usr/ports/; make index)
		    portversion -v -l \<
		}
		alias appsupgrade='pkgdb -F && BATCH=YES NO_CHECKSUM=YES portupgrade -a' ;;
	esac
esac


# 4. IntegratedDevelopmentEnvironment::ComputerTerminal::Screen
# -------------------------------------------------------------
REQUIRED_SCREEN_VERSION=4.0.3
function get-screen {
    case $OSTYPE in
        msys)
            pacman -S autoconf ncurses-devel libcrypt-devel
            cd ~
            wget http://ftp.gnu.org/gnu/screen/screen-${REQUIRED_SCREEN_VERSION}.tar.gz
            wget http://ftp.eq.uc.pt/software/pc/prog/cygwin/release/screen/screen-${REQUIRED_SCREEN_VERSION}-1-src.tar.bz2
            tar xvzf screen-${REQUIRED_SCREEN_VERSION}.tar.gz
            tar xvjf screen-${REQUIRED_SCREEN_VERSION}-1-src.tar.bz2
            sed -e 's/__CYGWIN__/__MSYS__/g' screen-${REQUIRED_SCREEN_VERSION}-1.src.patch > screen-${REQUIRED_SCREEN_VERSION}-1.src.patch.msys
            patch -p1 -d . < screen-${REQUIRED_SCREEN_VERSION}-1.cygwin.patch
            patch -p1 -d . < screen-${REQUIRED_SCREEN_VERSION}-1.src.patch.msys
            cd screen-${REQUIRED_SCREEN_VERSION}
            autoconf
            CFLAGS="-DNCURSES_STATIC" ./configure --prefix=/usr/local
            make
            make install
            mkdir -p /var/run
            touch /var/run/utmp
            touch /etc/ttys
            cd ~
            rm -rf screen-${REQUIRED_SCREEN_VERSION}*
    esac
}
if ! type -p screen > /dev/null ; then get-screen ; fi
# ### srcreen for opening ssh ###
function ssh_screen {
    eval server=\${$#}
    screen -t $server ssh "$@"
}
if [ x$TERM = xscreen ] ; then alias ssh=ssh_screen ; fi
# ### screen for status line ###
if [ "$TERM" = "screen" ]; then
    export LSCOLORS=gxfxcxdxbxegedabagacad
    export LS_COLORS='di=36;40:ln=35;40:so=32;40:pi=33;40:ex=31;40:bd=34;46:cd=34;43:su=0;41:sg=0;46:tw=0;42:ow=0;43:'
    # function chpwd { echo -n "_`dirs`\\" | ll }
    function chpwd { echo -n "_`dirs`\\" }
    function preexec {
        # see [zsh-workers:13180]
        # http://www.zsh.org/mla/workers/2000/msg03993.html
	emulate -L zsh
	local -a cmd ; cmd=(${(z)2})
	case $cmd[1] in
	    fg)
		if (( $#cmd == 1 )) ; then
		    cmd=(builtin jobs -l %+)
		else
		    cmd=(builtin jobs -l $cmd[2])
		fi ;;
	    %*) cmd=(builtin jobs -l $cmd[1]) ;;
	    cd) if (( $#cmd == 2)) ; then cmd[1]=$cmd[2] ; fi ;;
	    *) echo -n "k$cmd[1]:t\\" ; return ;;
	esac
	local -A jt; jt=(${(kv)jobtexts})
	$cmd >>(read num rest
	        cmd=(${(z)${(e):-\$jt$num}})
	        echo -n "k$cmd[1]:t\\") 2>/dev/null
    }
    chpwd
fi


# 4. IntegratedDevelopmentEnvironment::Chat::Slack
# ------------------------------------------------
function get-slackchat {
    case "${OSTYPE}" in
        darwin*|linux*)
            wget https://github.com/vektorlab/slackcat/releases/download/v0.7/slackcat-0.7-linux-amd64 -O ~/.local/bin/slackchat
            chmod +x ~/.local/bin/slackchat
        ;;
    esac
}


# 4. IntegratedDevelopmentEnvironment::CentOS
# -------------------------------------------
function get-centos {
    case "${OSTYPE}" in
        darwin*|linux*) docker run \
                               --name centos \
                               -p 22:22 \
                               -p 3000:3000 \
                               -p 3001:3001 \
                               -p 4000:4000 \
                               -p 8080:8080 \
                               -p 9000:9000 \
                               -p 9001:9001 \
                               -p 9002:9002 \
                               -p 9003:9003 \
                               -p 9004:9004 \
                               -p 9005:9005 \
                               -p 9006:9006 \
                               -p 9007:9007 \
                               -p 9008:9008 \
                               -p 9009:9009 \
                               -d \
                               centos:centos7 ;;
    esac
}
function set-centos {
    case "${OSTYPE}" in
        darwin*|linux*) docker restart centos ;;
    esac
}


# 5. Platform::Heroku
# -------------------
function get-heroku {
    case "${OSTYPE}" in
        freebsd*|darwin*) nix-install heroku-3.42.20 ;;
        linux*)
            case "${DIST}" in
                Redhat|RedHat|Debian) nix-install heroku-3.42.20 ;;
                Ubuntu)
                    parts install \
                          heroku_toolbelt \
                          hk ;;
            esac
    esac
}


# 5. Platform::GoogleCloudPlatform
# --------------------------------
export PATH="$HOME/google-cloud-sdk/bin:$PATH"
function get-gcloud {
    case "${OSTYPE}" in
        cygwin|darwin*|linux*)
            curl https://sdk.cloud.google.com | bash
            exec -l $SHELL ;;
    esac
}
function set-gcloud {
    case "${OSTYPE}" in
        cygwin|darwin*|linux*)
            source ~/google-cloud-sdk/completion.zsh.inc
            source ~/google-cloud-sdk/path.zsh.inc
            source ~/google-cloud-sdk/completion.zsh.inc ;;
    esac
}
if ! type -p gcloud > /dev/null ; then get-gcloud ; fi
if type -p gcloud > /dev/null ; then set-gcloud ; fi


# 5. Platform::GoogleCloudPlatform::GoogleContainerEngine
# --------------------------------------------------------
# #
# # CHEATSHEET
# #
# # 1. Pod
# kubectl get pod
# kubectl describe pod [pod-name]
# kubectl attach [pod-name]
# kubectl logs [-f] [pod-name] [-c container-name]
#
# # 2. Replication Controller
# kubectl get rc
# kubectl describe rc [rc-name]
# kubectl rolling-update [rc-name] [new-image-url]
# kubectl rolling-update [rc-name] -f [new-rc-schema-json-file-path]
# kubectl scale rc [rc-name] --replicas=[num]
#
# # 3. Service
# kubectl service service
# kubectl describe service [service-name]
#
# # 4. Cluster
# kubectl cluster-info
# gcloud compute instance-groups managed resize [gke-cluster-instance-group] --size [num]
#
# # 9. Create a secure tunnel ###
# ssh -f -nNT -L 8080:127.0.0.1:8080 core@<master-public-ip>
#
function get-kubectl {
    case $OSTYPE in
        darwin*)
            wget https://storage.googleapis.com/kubernetes-release/release/v1.0.1/bin/darwin/amd64/kubectl -O ~/.local/bin/kubectl
            chmod +x ~/.local/bin/kubectl ;;
        linux*)
            wget https://storage.googleapis.com/kubernetes-release/release/v1.0.1/bin/linux/amd64/kubectl -O  ~/.local/bin/kubectl
            chmod +x ~/.local/bin/kubectl ;;
    esac
}
if ! type kubectl > /dev/null ; then get-kubectl ; fi


# 5. Platform::AmazonWebServices
# ------------------------------
function get-aws {
    if ! type -p pip > /dev/null ; then get-pip ; fi
    case "${OSTYPE}" in
        freebsd*) ;;
        darwin*|linux*)
            pip install -U awscli s3cmd
            fix-compdef-problem
    esac
}
function fix-compdef-problem {
    touch ~/.zshenv
    echo 'autoload -Uz compinit
compinit' >> ~/.zshenv
}
if ! type -p aws > /dev/null ; then get-aws ; fi


# 9. Other::Customized
# --------------------
# ### dove ###
export PATH="$HOME/.local/dove/bin:$PATH"
function get-dove {
    git clone git@github.com:nabinno/dove.git ~/.local/dove
}
if [ ! -d ~/.local/dove ]; then get-dove; fi
function cd-dove {
    dove_path=`which dove`
    dove_dir=`dirname $dove_path`
    cd $dove_dir/../$1
}
alias zd=cd-dove
# ### dotfiles ###
function get-dotfiles {
    while getopts ":c:m:ih" opt ; do
        case $opt in
            "c") is_credential=true ;;
            "m") is_mu4e=true ;;
            "e") is_esa=true ;;
            "r") is_irc=true ;;
            "i") is_init_el=true ;;
            "h") echo ''
                 echo "Usage: get-dotfiles [-mih]" 1>&2
                 is_exit=true ;;
            "?") echo "$0: Invalid option -$OPTARG" 1>&2
                 echo "Usage: $0 [-mih]" 1>&2
                 is_exit=true ;;
        esac
    done
    if ! [ $is_exit ] ; then
        cd ~/; wait
        if [ ! -d ~/.local/dotfiles ]; then
            mkdir -p ~/.local
            sh -c "$(curl -fsSL https://raw.github.com/nabinno/dotfiles/master/install)"; wait
        fi
        cd ~/.local/dotfiles
        git checkout -- .
        git checkout develop
        git pull
        rm -rf                         .emacs.d/lisp/*;    wait
        cp -pr ~/.emacs.d/lisp/*       .emacs.d/lisp/;     wait
        cp -pr ~/.emacs.d/bin/*        .emacs.d/bin/;      wait
        cp -pr ~/.emacs.d/eshell/alias .emacs.d/eshell/;   wait
        cp -pr ~/.emacs.d/init.el      .emacs.d/;          wait
        cp -pr ~/.offlineimap.py .
        cp -pr ~/.aspell.conf .
        cp -pr ~/.zshenv .
        cp -pr ~/.zshrc .
        cp -pr ~/.mysql .
        cp -pr ~/.nginx .
        rm -rf ~/.nginx/conf.d
        case "${OSTYPE}" in (freebsd*|darwin*|linux*) cp -pr ~/.screenrc . ;; esac
        if ! [ $is_credential ] ; then git checkout -- .emacs.d/lisp/init-credential.el ; fi
        if ! [ $is_mu4e       ] ; then git checkout -- .emacs.d/lisp/init-mu4e.el       ; fi
        if ! [ $is_esa        ] ; then git checkout -- .emacs.d/lisp/init-esa.el        ; fi
        if ! [ $is_irc        ] ; then git checkout -- .emacs.d/lisp/init-irc.el        ; fi
        if ! [ $is_init_el    ] ; then git checkout -- .emacs.d/init.el                 ; fi
    fi
}
alias zg='get-dotfiles'
function put-dotfiles {
    # pre proc
    local current_pwd=`pwd`
    cd ~/
    if [ ! -d ~/.local/dotfiles ]; then
        mkdir -p ~/.local
        sh -c "$(curl -fsSL https://raw.github.com/nabinno/dotfiles/master/install)"; wait
    fi
    cd ~/.local/dotfiles; wait
    git checkout -- .
    git checkout develop
    git pull
    rm -rf .emacs.d/lisp/init-mu4e.el
    rm -rf .emacs.d/lisp/init-esa.el
    rm -rf .emacs.d/lisp/init-credential.el
    # main proc
    cp -pr .emacs.d/lisp/*       ~/.emacs.d/lisp/;   wait
    cp -pr .emacs.d/bin/*        ~/.emacs.d/bin/;    wait
    cp -pr .emacs.d/eshell/alias ~/.emacs.d/eshell/; wait
    cp -pr .emacs.d/init.el      ~/.emacs.d/;        wait
    cp -pr .offlineimap.py ~/;                       wait
    cp -pr .aspell.conf ~/;                          wait
    cp -pr .zshenv ~/;                               wait
    cp -pr .zshrc ~/;                                wait
    cp -pr .mysql ~/;                                wait
    cp -pr .nginx ~/;                                wait
    case "${OSTYPE}" in
        freebsd*|darwin*|linux*) cp -pr .screenrc ~/; wait ;;
    esac
    # post proc
    git checkout -- .emacs.d/lisp/init-mu4e.el
    git checkout -- .emacs.d/lisp/init-esa.el
    git checkout -- .emacs.d/lisp/init-credential.el
    cd $current_pwd
    exec -l zsh
}
alias zp=put-dotfiles
# ### other ###
function bkup { cp -ipr $1 $1.org$(date +%y%m%d) }
function bkup-targz { tar zcvf $2$(date +%y%m%d)_$1_$(date +%H).tar.gz $3$1 }
alias b='bkup'
alias bU='bkup-targz'
alias bin='~/bin'
alias c='/bin/cp -ipr'
alias d='/bin/rm -fr'
alias du="du -h"
alias df="df -h"
# alias grep='egrep -ano'
# alias egrep='egrep --color=auto'
alias egrep='\egrep -H -n'
alias fgrep='fgrep --color=auto'
alias grep='grep --color=auto'
function gresreg {
    for i in $(\grep -lr $1 *) ; do
	cp $i $i.tmp;
	sed -e "s/$1/$2/g" $i.tmp > $i;
	rm $i.tmp;
    done
}
alias e='emacsclient -t'
alias ij='jobs -l'
alias ic='cat /proc/cpuinfo'
alias in='netstat -a -n | more'
alias im='cat /proc/meminfo'
case "${OSTYPE}" in
    freebsd*|darwin*) alias ip="ps aux" ;;
    cygwin|msys) alias ip="ps -flW" ;;
    linux*)
        case "${DIST}" in
            Redhat|RedHat) alias ip="ps aux" ;;
            Debian|Ubuntu) alias ip="ps aux" ;;
        esac
esac
alias it="date -R"
alias j='cd'
alias k='/bin/mkdir -p'
function kl { kill -f $1 }
function chpwd { }
#function chpwd { ll }
function lower {
    for i in "$@" ; do
        \mv -f $i $(echo $i | tr "[:upper:]" "[:lower:]")
    done
}
alias lw='lower'
alias m='/bin/mv'
alias mk="make config install distclean"
alias MK="make deinstall"
alias pwd='pwd -P'
function upper {
    for i in * ; do
        \mv -f $i $(echo $i | tr "[:lower:]" "[:upper:]")
    done
}
alias up="upper"
alias r='/bin/mv'
alias re='e ~/.zshrc'
alias reb='cp -ip ~/.zshrc ~/.zshrc.org$(date +%y%m%d)'
function rename {
    for i in *$1* ; do
        \mv -f $i `echo $i | sed -e s,$1,$2,g`
    done
}
alias rn="rename"
function rename-recursively {
    find . -print | while read file ; do
        \mv -f $file ${file//$1/$2}
    done
}
alias rnr="rename-recursively"
function rr { exec -l zsh }
function rename {
    for i in *$1* ; do
        \mv -f $i    # (echo $i | sed -e s,$1,$2,g)
    done
}
alias rn="rename"
function rename-recursively {
    find . -print | while read file ; do
        \mv -f $file ${file//$1/$2}
    done
}
alias rnr="rename-recursively"
alias s='/bin/ln -s'
alias scp='/usr/bin/scp -Cpr'
# alias su="su -l"
alias u='tar zxvf'
alias U='tar zcvf $1.tar.gz $1'
alias uz='unzip'
alias v="cat"
function t { \mv (.*~|.*.org*|*.org*|*.tar.gz|*.stackdump|*.tar.gz|*.asx|*.0|*.msi|*.wav|*.doc|*.pdf|$1) .old/ }
# ### other source file ###
if [ -f ~/.zshrc.mine ]; then source ~/.zshrc.mine; fi



