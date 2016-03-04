# 1. BasicSettings::OsDetect
# 1. BasicSettings::EnvironmentVariable
# 1. BasicSettings::EnvironmentVariable::Locale
# 1. BasicSettings::EnvironmentVariable::Local
# 1. BasicSettings::BaseInstallation
# 1. BasicSettings::PackageManager::Nix
# 1. BasicSettings::PackageManager::Autoparts
# 1. BasicSettings::PackageManager::Homebrew
# 1. BasicSettings::PackageManager::WindowsManagementFramework
# 1. BasicSettings::PackageManager::Anyenv
# 2. ProgrammingLanguage::Ruby
# 2. ProgrammingLanguage::Elixir
# 2. ProgrammingLanguage::Go
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
# 4. IntegratedDevelopmentEnvironment::ResourceManagement::Filesystem
# 4. IntegratedDevelopmentEnvironment::ResourceManagement::Git
# 4. IntegratedDevelopmentEnvironment::ResourceManagement::PlantUml
# 4. IntegratedDevelopmentEnvironment::SoftwareDebugging::Benchmark
# 4. IntegratedDevelopmentEnvironment::OsLevelVirtualization::Vagrant
# 4. IntegratedDevelopmentEnvironment::SoftwareDeployment::Docker
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
case "${OSTYPE}" in
    freebsd*|darwin*|linux*)
        [ -z "$PS1" ] && return
        stty -ixon
        stty start undef
        stty stop undef
        OS=`uname -s`
        REV=`uname -r`
        MACH=`uname -m`
        function GetVersionFromFile () {
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


# 1. BasicSettings::EnvironmentVariable
# -------------------------------------
# export EDITOR=/usr/local/bin/vi
# export LANG=ja_JP.UTF-8
# export LANG=ja_JP.eucJP
alias egrep='egrep --color=auto'
alias fgrep='fgrep --color=auto'
alias grep='grep --color=auto'
export CLICOLOR=1
export EDITOR='vim -f'
export LANG=en_US.UTF-8
export LANGUAGE=en_US.UTF-8
export LC_ALL=en_US.UTF-8
export LC_CTYPE=UTF-8
export LC_MESSAGES=C
export MAILPATH=$HOME/MailBox/postmaster/maildir
export MANPATH=$HOME/.linuxbrew/share/man:$MANPATH
export INFOPATH=$HOME/.linuxbrew/share/info:$INFOPATH
export LD_LIBRARY_PATH=$HOME/.linuxbrew/lib:$LD_LIBRARY_PATH
export PATH=$HOME/bin:$HOME/local/bin:$PATH
export PATH="/opt/local/bin:$PATH"
export PATH="/opt/local/sbin:$PATH"
export PATH="$HOME/.local/bin:$PATH"
export PATH="$HOME/.local/exenv/bin:$PATH"
export PATH="$HOME/.local/dove/bin:$PATH"
export PATH="$HOME/.anyenv/bin:$PATH"
export PATH="$HOME/.parts/autoparts/bin:$PATH"
export PATH="$HOME/.parts/lib/node_modules/less/bin:$PATH"
export PATH="$HOME/.parts/packages/python2/$REQUIRED_PYTHON_VERSION/bin:$PATH"
export PATH="$HOME/.parts/packages/python2/$REQUIRED_PYTHON_VERSION/bin:$PATH"
export PATH="$HOME/.linuxbrew/bin:$PATH"
export PATH="$HOME/.cask/bin:$PATH"
export PATH="$HOME/.local/google-cloud-sdk/bin:$PATH"
export PATH="$HOME/.local/perl-$REQUIRED_PERL_VERSION/bin:$PATH"
export PS1="\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;36m\]\w\[\033[00m\]\$(parse_git_branch)\$ "


# 1. BasicSettings::Locale
# ------------------------
function get-ntp () {
    case "${OSTYPE}" in
        linux*)
            case "${DIST}" in
                Redhat|RedHat) sudo yum install -y ntp ;;
                Debian|Ubuntu) sudo apt-get install -y ntp ;;
            esac
    esac
}
function set-locale () {
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
function get-base () {
    case "${OSTYPE}" in
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
function get-wget () {
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
function get-rlwrap () {
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
function get-autoconf () {
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
function get-boost () {
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
function gnu-get () {
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


# 1. BasicSettings::PackageManager::Nix
# -------------------------------------
function get-nix () {
    case "${OSTYPE}" in
        darwin*) cd ~ && curl https://nixos.org/nix/install | sh ;;
        linux*)
            case $DIST in
                Redhat|RedHat|Debian) cd ~ && curl https://nixos.org/nix/install | sh ;;
                Ubuntu)
                    case $DIST_VERSION in
                        14.04) cd ~ && curl https://nixos.org/nix/install | sh ;;
                        12.04)
                            cd ~ && curl https://nixos.org/nix/install | sh
                            nix-channel --add http://nixos.org/channels/nixpkgs-unstable
                            nix-channel --update
                            sudo groupadd -g 20000 nixbld
                            for i in `seq 1 10` ; do
                                sudo useradd -u `expr 20000 + $i` -g nixbld -c "Nix build user $i" -d /var/empty -s /noshell
                            done
                            sudo echo "build-users-group = nixbld" >> /etc/nix/nix.conf
                            sudo chown -R vagrant /nix
                            source ~/.nix-profile/etc/profile.d/nix.sh ;;
                    esac
            esac
    esac
}
function get-nix-packages () {
    case "${OSTYPE}" in
        darwin*|linux*)
            nix-build \
                heroku_toolbelt \
                phantomjs \
                the_silver_searcher \
                tree \
                uuid ;;
    esac
}
if [ -f ~/.nix-profile/etc/profile.d/nix.sh ] ; then source ~/.nix-profile/etc/profile.d/nix.sh ; fi
if [ ! -f ~/.nix-profile/etc/profile.d/nix.sh ] ; then get-nix ; fi
if [ "$OSTYPE" = "linux*" ] && [ "$DIST" = "Ubuntu"] && [ "$DIST_VERSION" = "12.04" ] ; then alias nix-env='sudo nix-env' ; fi
alias nix-install='nix-env --install '
alias nix-uninstall='nix-env --uninstall '
alias nix-search='nix-env -qa | \grep '
alias nix-list='nix-env -q --installed'


# 1. BasicSettings::PackageManager::Autoparts
# -------------------------------------------
function get-parts () {
    case "${OSTYPE}" in
        linux*)
            case "${DIST}" in
                Debian|Ubuntu)
                    get-base
                    ruby -e "$(curl -fsSL https://raw.github.com/nitrous-io/autoparts/master/setup.rb)"
                    eval "$(parts env)"
                    get-parts-packages ;;
            esac
    esac
}
function get-parts-packages () {
    case "${OSTYPE}" in
        linux*)
            case "${DIST}" in
                Debian|Ubuntu)
                    parts install \
                          heroku_toolbelt \
                          phantomjs \
                          the_silver_searcher \
                          tree \
                          uuid ;;
            esac
    esac
}
if ! type -p parts > /dev/null ; then ; get-parts ; fi
if type -p parts > /dev/null ; then ; eval "$(parts env)" ; fi


# 1. BasicSettings::PackageManager::Homebrew
# ------------------------------------------
function get-brew () {
    case "${OSTYPE}" in
        darwin*) ;;
        linux*)
            ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/linuxbrew/go/install)"
            get-brew-packages
    esac
}
function get-brew-packages () {
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
if ! type -p brew > /dev/null ; then get-brew ; fi


# 1. BasicSettings::PackageManager::WindowsManagementFramework
# ------------------------------------------------------------
if [ "$OSTYPE" = "cygwin" ] ; then
    function abstract-powershell () {
        local args=$(echo $*)
        powershell /c $args
    }
    function get-package ()         { abstract-powershell $0 $* }
    function get-packageprovider () { abstract-powershell $0 $* }
    function find-package ()        { abstract-powershell $0 $* }
    function install-package ()     { abstract-powershell $0 $* }
    function uninstall-package ()   { abstract-powershell $0 $* }
    function get-apt-cyg () {
        wget https://raw.githubusercontent.com/transcode-open/apt-cyg/master/apt-cyg -O /usr/local/bin/apt-cyg
        chmod 755 apt-cyg
        get-global-aptcyg-packages
    }
    function get-global-aptcyg-packages () {
        apt-cyg install \
                base-cygwin \
                base-files \
                bash \
                binutils \
                cron \
                curl \
                cygrunsrv \
                cygutils \
                cygwin \
                cygwin-devel \
                findutils \
                gawk \
                git \
                gnuplot \
                grep \
                hostname \
                info \
                less \
                mintty \
                ping \
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
                which \
                whois \
                wget \
                zip \
                zoo \
                zsh
    }
    if ! apt-cyg > /dev/null ; then get-apt-cyg ; fi
    function get-wmf () {
        find-package && get-packageprovider -name chocolatey
    }
    function get-global-wmf-packages () {
        install-package FoxitReader
        install-package Gpg4win
        install-package InkScape
        install-package IrfanView
        install-package WinSplitRevolution
        install-package googledrive
        install-package 7zip
        install-package f.lux
        install-package terminals
        install-package vagrant
    }
fi


# 1. BasicSettings::PackageManager::Anyenv
# ----------------------------------------
function get-anyenv () {
    case "${OSTYPE}" in
        freebsd*|darwin*|linux*)
            git clone https://github.com/riywo/anyenv ~/.anyenv
            eval "$(anyenv init -)" ;;
    esac
}
if ! type -p anyenv > /dev/null; then get-anyenv ; fi
if type -p anyenv > /dev/null; then eval "$(anyenv init -)" ; fi


# 2. ProgrammingLanguage::Ruby
# ----------------------------
REQUIRED_RUBY_VERSION=2.2.0
# ### version control ###
function get-rbenv () {
    case "${OSTYPE}" in
        freebsd*|darwin*|linux*) anyenv install rbenv && exec -l zsh ;;
    esac
}
# ### installation ###
function get-ruby () {
    case "${OSTYPE}" in
        cygwin) apt-cyg install ruby ;;
        freebsd*|darwin*|linux*)
            rbenv install $REQUIRED_RUBY_VERSION
            rbenv rehash
            rbenv global $REQUIRED_RUBY_VERSION
            get-global-gem-packages ;;
    esac
}
function get-global-gem-packages () {
    gem install \
        bundler \
        compass \
        haml \
        html2slim \
        rails \
        rubygems-bundler \
        sidekiq \
        unicorn \
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
REQUIRED_ERLANG_VERSION=18.2
REQUIRED_ELIXIR_VERSION=1.2.0
REQUIRED_PHOENIXFRAMEWORK_VERSION=1.1.1
# ### version control ###
function get-kerl () {
    case "${OSTYPE}" in
        freebsd*|darwin*|linux*)
            curl https://raw.githubusercontent.com/yrashk/kerl/master/kerl -o ~/.local/bin/kerl
            chmod a+x ~/.local/bin/kerl
    esac
}
if ! type -p kerl > /dev/null ; then get-kerl ; fi
function get-exenv () {
    case "${OSTYPE}" in
        freebsd*|darwin*|linux*)
            rm -fr ~/.local/exenv
            git clone https://github.com/mururu/exenv.git ~/.local/exenv
            git clone https://github.com/mururu/elixir-build.git
            cp -f elixir-build/bin/*  ~/.local/exenv/bin/
            rm -fr elixir-build
            eval "$(exenv init -)" ;;
    esac
}
if ! type -p exenv > /dev/null ; then get-exenv ; fi
if type -p exenv > /dev/null ; then eval "$(exenv init -)" ; fi
# ### installation ###
function get-erlang () {
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
function get-elixir () {
    case "${OSTYPE}" in
        freebsd*|darwin*|linux*)
            exenv install $REQUIRED_ELIXIR_VERSION
            exenv rehash
            exenv global $REQUIRED_ELIXIR_VERSION
            get-mix-packages ;;
    esac
}
if ! type -p iex > /dev/null ; then get-elixir ; fi
function get-mix-packages () {
    mix local.hex
    mix archive.install https://github.com/phoenixframework/phoenix/releases/download/v$REQUIRED_PHOENIXFRAMEWORK_VERSION/phoenix_new-$REQUIRED_PHOENIXFRAMEWORK_VERSION.ez
}


# 2. ProgrammingLanguage::Go
# --------------------------
# ### version control ###
function get-goenv () {
    case "${OSTYPE}" in
        freebsd*|darwin*|linux*) anyenv install goenv && exec -l zsh ;;
    esac
}
if ! type -p goenv > /dev/null ; then get-goenv ; fi
function get-go () {
    case "${OSTYPE}" in
        freebsd*) ;;
        darwin*) ;;
        linux*)
            case "${DIST}" in
                Redhat|RedHat) ;;
                Debian) ;;
                Ubuntu)
                    case "$DIST_VERSION" in
                        12.04) parts install go ;;
                        14.04) ;;
                    esac
            esac
    esac
}
if ! type -p go > /dev/null ; then get-go ; fi


# 2. ProgrammingLanguage::Java
# ----------------------------
REQUIRED_OPENJDK_VERSION=8u76b00
REQUIRED_OEPNJDK_SHORT_VERSION=1.8
REQUIRED_PLAY_VERSION=2.2.3
export PLAY_HOME=/usr/local/play-$REQUIRED_PLAY_VERSION
export PATH="$PLAY_HOME:$PATH"
# ### version control ###
function get-jenv () {
    case "${OSTYPE}" in
        freebsd*|darwin*|linux*) anyenv install jenv && exec -l zsh ;;
    esac
}
if ! type -p jenv > /dev/null ; then get-jenv ; fi
# ### installation ###
function get-java () {
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
                            sudo apt-get update && sudo apt-get install -y openjdk-8-jdk
                            jenv add /usr/lib/jvm/java-1.8.0-openjdk-amd64/
                            jenv global $REQUIRED_OEPNJDK_SHORT_VERSION ;;
                        14.04)
                            nix-install openjdk-$REQUIRED_OPENJDK_VERSION
                            jenv add ~/.nix-profile/lib/openjdk
                            jenv global $REQUIRED_OEPNJDK_SHORT_VERSION ;;
                    esac
            esac
    esac
}
function set-javahome () {
    case "${OSTYPE}" in
        freebsd*|darwin*|linux*)
            export JAVA_HOME=~/.nix-profile
            jenv global $REQUIRED_OEPNJDK_SHORT_VERSION ;;
    esac
}
function get-play () {
    case "${OSTYPE}" in
        freebsd*|darwin*|linux*)
            wget http://downloads.typesafe.com/play/$REQUIRED_PLAY_VERSION/play-$REQUIRED_PLAY_VERSION.zip
            unzip play-$REQUIRED_PLAY_VERSION.zip
            mv play-$REQUIRED_PLAY_VERSION ~/.local/
            rm -fr play-$REQUIRED_PLAY_VERSION.zip ;;
    esac
}
function get-sbt () {
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
function get-phpenv () {
    case "${OSTYPE}" in
        freebsd*|darwin*|linux*) anyenv install phpenv && exec -l zsh ;;
    esac
}
if ! type -p phpenv > /dev/null ; then get-phpenv ; fi
# ### installation ###
REQUIRED_PHP_VERSION=5.6.16
function get-php () {
    case "${OSTYPE}" in
        freebsd*|darwin*) nix-install php-$REQUIRED_PHP_VERSION ;;
        linux*)
            case "${DIST}" in
                Debian) nix-install php-$REQUIRED_PHP_VERSION ;;
                Ubuntu)
                    case "$DIST_VERSION" in
                        12.04) parts install \
                                     php5 \
                                     composer \
                                     phpunit ;;
                        14.04) nix-install php-$REQUIRED_PHP_VERSION ;;
                    esac
            esac
    esac
}
if ! type -p php > /dev/null; then get-php; fi
function get-fastcgi () {
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
if [ ! -f /etc/init.d/php-fastcgi ] && [ ! -f /etc/init.d/php-fpm ] && [ ! -f /etc/init.d/php5-fpm ] && ! type -p php-fpm > /dev/null ; then
    get-fastcgi
fi
function php-fastcgid () {
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
function fastcgi-restart () {
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
# ### version control ###
function get-pyenv () {
    case "${OSTYPE}" in
        freebsd*|darwin*|linux*) anyenv install pyenv && exec -l zsh ;;
    esac
}
if ! type -p pyenv > /dev/null ; then get-pyenv ; fi
# ### installation ###
function get-python () {
    case "${OSTYPE}" in
        cygwin) apt-cyg install python ;;
        freebsd*|darwin*|linux*)
            pyenv install $REQUIRED_PYTHON_VERSION
            pyenv rehash
            pyenv global $REQUIRED_PYTHON_VERSION
    esac
}
if ! type -p python > /dev/null; then get-python ; fi
function get-pip () {
    case "${OSTYPE}" in
        freebsd*|darwin*|linux*)
            easy_install pip
            get-global-pip-packages ;;
    esac
}
function get-global-pip-packages () {
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
REQUIRED_PERL_VERSION=5.18.2
# ### version control ###
function get-plenv () {
    case "${OSTYPE}" in
        freebsd*|darwin*|linux*) anyenv install plenv && exec -l zsh ;;
    esac
}
if ! type -p plenv > /dev/null ; then get-plenv ; fi
# ### installation ###
function get-perl () {
    case "${OSTYPE}" in
        cygwin) apt-cyg install perl ;;
        freebsd*|darwin*|linux*)
            plenv install $REQUIRED_PERL_VERSION
            plenv rehash
            plenv global $REQUIRED_PERL_VERSION
            plenv install-cpanm
    esac
}
if ! type -p perl > /dev/null ; then get-perl ; fi
# ### plagger ###
function get-global-cpan-packages () {
    cpanm -fi \
          # plagger
          YAML::Loader \
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
          Plagger \
          # other
          Carton
}
# ### cpan ###
function cpanmodulelist () { perl -e "print \"@INC\"" | find -name "*.pm" -print }
function cpanmoduleversion () { perl -M$1 -le "print \$$1::VERSION" }
function cpan-uninstall () { perl -MConfig -MExtUtils::Install -e '($FULLEXT=shift)=~s{-}{/}g;uninstall "$Config{sitearchexp}/auto/$FULLEXT/.packlist",1' }
alias cpanmini='cpan --mirror ~/.cpan/minicpan --mirror-only'
# alias cpan-uninstall='perl -MConfig -MExtUtils::Install -e '"'"'($FULLEXT=shift)=~s{-}{/}g;uninstall "$Config{sitearchexp}/auto/$FULLEXT/.packlist",1'"'"
# eval $(perl -I$HOME/.local/lib/perl5 -Mlocal::lib=$HOME/.local)
# export PKG_DBDIR=$HOME/local/var/db/pkg
# export PORT_DBDIR=$HOME/local/var/db/pkg
# export INSTALL_AS_USER
# export LD_LIBRARY_PATH=$HOME/local/lib
# export TMPDIR=$HOME/local/tmp
# export MODULEBUILDRC=$HOME/local/.modulebuildrc
# export PERL_MM_OPT="INSTALL_BASE=$HOME/local"
# export PERL5LIB=$HOME/local/lib/perl5:$PERL5LIB
# export PERL_CPANM_OPT="-l ~/local --mirror http://ftp.funet.fi/pub/languages/perl/CPAN/"
# export PERL_CPANM_OPT="-l ~/local --mirror ~/.cpan/minicpan/"


# 2. ProgrammingLanguage::Javascript
# ----------------------------------
export REQUIRED_NODE_VERSION='4.2.4'
export node='NODE_NO_READLINE=1 node'
# ### version control ###
function get-ndenv () {
    case "${OSTYPE}" in
        freebsd*|darwin*|linux*) anyenv install ndenv && exec -l zsh ;;
    esac
}
if ! type -p ndenv > /dev/null ; then get-ndenv ; fi
# ### installation ###
function get-node () {
    case "$OSTYPE" in
        cygwin) install-package nodejs ;;
        linux*)
            ndenv install v$REQUIRED_NODE_VERSION
            ndenv rehash
            ndenv global v$REQUIRED_NODE_VERSION
            get-global-npm-packages ;;
    esac
}
function set-node () {
    case "$OSTYPE" in
        linux*) ndenv global v$REQUIRED_NODE_VERSION
    esac
}
function get-global-npm-packages () {
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
function rebuild-sass () {
    npm uninstall --save-dev gulp-sass
    npm install --save-dev gulp-sass@2
    npm rebuild node-sass
}


# 2. ProgrammingLanguage::RemoteProcedureCall
# -------------------------------------------
function get-protobuf () {
    case "${OSTYPE}" in
        freebsd*|darwin*|linux*) nix-install protobuf-2.6.1 ;;
    esac
}
function get-thrift () {
    case "${OSTYPE}" in
        freebsd*|darwin*|linux*) nix-install thrift-0.9.2 ;;
    esac
}
if ! type -p protoc > /dev/null ; then get-protobuf ; fi
if ! type -p thrift > /dev/null ; then get-thrift ; fi


# 3. Daemon::Database::Postgresql
# -------------------------------
REQUIRED_POSTGRESQL_VERSION=9.4.5
PSQL_PAGER='less -S'
# ### installation ###
function get-postgresql () {
    case "${OSTYPE}" in
        darwin*) nix-install postgresql-$REQUIRED_POSTGRESQL_VERSION ;;
        linux*)
            case "${DIST}" in
                Redhat|RedHat|Debian) nix-install postgresql-$REQUIRED_POSTGRESQL_VERSION ;;
                Ubuntu) parts install postgresql ;;
            esac
    esac
}
if ! type -p psql > /dev/null ; then get-postgresql ; fi
function pg-restart () {
    case "${OSTYPE}" in
        darwin*)
            sudo service postgresql stop
            sudo service postgresql start
            sudo service postgresql status ;;
        linux*)
            case "${DIST}" in
                Redhat|RedHat)
                    sudo service postgresql stop
                    sudo service postgresql start
                    sudo service postgresql status ;;
                Debian) ;;
                Ubuntu)
                    parts restart postgresql
                    parts status postgresql ;;
            esac
    esac
}
function pg-status () {
    case "${OSTYPE}" in
        darwin*) sudo service postgresql status ;;
        linux*)
            case "${DIST}" in
                Redhat|RedHat) sudo service postgresql status ;;
                Debian) ;;
                Ubuntu) parts status postgresql ;;
            esac
    esac
}
alias pgr="pg-restart"
alias pgp="ps aux | \grep -G 'postgresql.*'"
alias pgs="pg-status"
alias pgk="sudo killall postgresql"
alias psql='rlwrap -a -pCYAN -if ~/.local/rlwrap/sqlplus psql'


# 3. Daemon::Database::Mysql
# --------------------------
REQUIRED_MYSQL_VERSION=5.5.45
# ### installation ###
function get-mysql () {
    case "${OSTYPE}" in
        darwin*) nix-install mysql-$REQUIRED_MYSQL_VERSION && set-mysql ;;
        linux*)
            case "${DIST}" in
                Redhat|RedHat|Debian) nix-install mysql-$REQUIRED_MYSQL_VERSION && set-mysql ;;
                Ubuntu)
                    case $DIST_VERSION in
                        12.04)
                            sudo apt-get -y remove mysql-server
                            sudo apt-get -y autoremove
                            sudo apt-get -y install software-properties-common
                            sudo add-apt-repository -y ppa:ondrej/mysql-$REQUIRED_MYSQL_VERSION
                            sudo apt-get update
                            sudo apt-get -y install mysql-server ;;
                        14.04) nix-install mysql-$REQUIRED_MYSQL_VERSION && set-mysql ;;
                    esac
            esac
    esac
}
if ! type -p mysql > /dev/null ; then get-mysql ; fi
function set-mysql () {
    echo "innodb_file_format = Barracuda" | sudo tee -a /etc/mysql/my.cnf
    echo "innodb_file_per_table = 1"      | sudo tee -a /etc/mysql/my.cnf
    echo "innodb_large_prefix"            | sudo tee -a /etc/mysql/my.cnf
}
function my-restart () {
    case "${OSTYPE}" in
        darwin*)
            sudo service mysql stop
            sudo service mysql start
            sudo service mysql status ;;
        linux*)
            case "${DIST}" in
                Redhat|RedHat|Debian)
                    sudo service mysql stop
                    sudo service mysql start
                    sudo service mysql status ;;
                Ubuntu)
                    case $DIST_VERSION in
                        12.04)
                            sudo /etc/init.d/mysql restart
                            sudo /etc/init.d/mysql status ;;
                        14.04)
                            sudo service mysql stop
                            sudo service mysql start
                            sudo service mysql status ;;
                    esac
            esac
    esac
}
function my-status () {
    case "${OSTYPE}" in
        darwin*) sudo service mysql status ;;
        linux*)
            case "${DIST}" in
                Redhat|RedHat|Debian) sudo service mysql status ;;
                Ubuntu)
                    case $DIST_VERSION in
                        12.04) sudo /etc/init.d/mysql status ;;
                        14.04) sudo service mysql status ;;
                    esac
            esac
    esac
}
alias mr="my-restart"
alias mp="ps aux | \grep -G 'mysql.*'"
alias ms="my-status"
alias mk="sudo killall mysqld"
alias mysql="rlwrap -a -pCYAN -if ~/.local/rlwrap/sqlplus mysql -uroot --pager='less -S'"


# 3. Daemon::Database::Redis
# --------------------------
REQUIRED_REDIS_VERSION=3.0.2
# ### installation ###
function get-redis () {
    case "${OSTYPE}" in
        darwin*)
            nix-install redis-$REQUIRED_REDIS_VERSION
            nohup redis-server >/dev/null 2>&1 </dev/null & ;;
        linux*)
            nix-install redis-$REQUIRED_REDIS_VERSION
            nohup redis-server >/dev/null 2>&1 </dev/null & ;;
    esac
}
if ! type -p redis-cli > /dev/null ; then get-redis ; fi
function redis-restart () {
    case "${OSTYPE}" in
        darwin*) ;;
        linux*)
            sudo pkill redis-server
            nohup redis-server >/dev/null 2>&1 </dev/null &
            ps aux | \grep -G 'redis.*' ;;
    esac
}
function redis-stop () {
    case "${OSTYPE}" in
        darwin*) ;;
        linux*) sudo pkill redis-server ;;
    esac
}
function redis-status () {
    case "${OSTYPE}" in
        darwin*) ;;
        linux*) ps aux | \grep -G 'redis.*' ;;
    esac
}
alias redis-cli='rlwrap -a -pCYAN -if ~/.local/rlwrap/sqlplus redis-cli'
alias redis-cli-monitor='redis-cli monitor'
alias redis-cli-info='redis-cli info'
alias redis-cli-dump='redis-cli bgsave'
alias rdr="redis-restart"
alias rdp="redis-status"
alias rds="redis-status"
alias rdi="redis-cli-info"
alias rdm="redis-cli-monitor"
alias rdk="redis-stop"


# 3. Daemon::Database::Memcached
# ------------------------------
REQUIRED_MEMCACHED_VERSION=1.4.20
function get-memcached () {
    case "${OSTYPE}" in
        darwin*) ;;
        linux*) nix-install memcached-$REQUIRED_MEMCACHED_VERSION ;;
    esac
}
if ! type -p memcached > /dev/null ; then get-memcached ; fi
function get-memcached-tool() {
    wget https://raw.githubusercontent.com/memcached/memcached/master/scripts/memcached-tool -O ~/.local/bin/memcached-tool
    chmod +x ~/.local/bin/memcached-tool
}
if ! type -p memcached-tool > /dev/null ; then get-memcached-tool ; fi
function memcached-restart () {
    case "${OSTYPE}" in
        darwin*) ;;
        linux*)
            case $DIST in
                Redhat|RedHat)
                    sudo service memcached stop
                    sudo service memcached start
                    sudo service memcached status ;;
                Debian|Ubuntu)
                    sudo pkill memcached
                    nohup memcached >/dev/null 2>&1 </dev/null &
                    ps aux | \grep -G 'memcached.*' ;;
            esac
    esac
}
function memcached-stop () {
    case "${OSTYPE}" in
        darwin*) ;;
        linux*)
            case "${DIST}" in
                Redhat|RedHat) sudo service memcached stop ;;
                Debian|Ubuntu) sudo /usr/bin/pkill memcached ;;
            esac
    esac
}
function memcached-status () {
    case "${OSTYPE}" in
        darwin*) ;;
        linux*)
            case $DIST in
                Redhat|RedHat) sudo service memcached status ;;
                Debian|Ubuntu) ps aux | \grep -G 'memcached.*' ;;
            esac
    esac
}
alias memcached-monitor='memcached-tool 127.0.0.1:11211 display'
alias memcached-info='memcached-tool 127.0.0.1:11211 stats'
alias memcached-dump='memcached-tool 127.0.0.1:11211 dump'
alias mcr="memcached-restart"
alias mcp="memcached-status"
alias mcs="memcached-status"
alias mci="memcached-info"
alias mcm="memcached-monitor"
alias mck="memcached-stop"



# 3. Daemon::HttpServer::Nginx
# ----------------------------
REQUIRED_NGINX_VERSION=1.9.9
function get-nginx () {
    case "${OSTYPE}" in
        darwin*|linux*) nix-install nginx-$REQUIRED_NGINX_VERSION ;;
    esac
}
function set-nginx () {
    useradd -s /bin/false nginx
    sudo sed -i "s|^\(#user  nobody;\)|#\1\nuser  nginx;|g"                                                      ~/.nix-profile/conf/nginx.conf
    sudo sed -i "s|^\(worker_processes  1;\)|#\1\nworker_processes  auto;|g"                                     ~/.nix-profile/conf/nginx.conf
    sudo sed -i "s|^#\(log_format  main  '$remote_addr - $remote_user [$time_local] \"$request\" '\)|\1|g"       ~/.nix-profile/conf/nginx.conf
    sudo sed -i "s|^#\(                  '$status $body_bytes_sent \"$http_referer\" '\)|\1|g"                   ~/.nix-profile/conf/nginx.conf
    sudo sed -i "s|^#\(                  '\"$http_user_agent\" \"$http_x_forwarded_for\"';\)|\1|g"               ~/.nix-profile/conf/nginx.conf
    sudo sed -i "s|^\(#gzip  on;\)|\1\ninclude/etc/nginx/conf.d/\*.conf;\ninclude/etc/nginx/sites-enabled/\*;|g" ~/.nix-profile/conf/nginx.conf
    sudo mkdir -rf /etc/nginx/sites-enabled
    sudo mkdir -rf /etc/nginx/sites-available
    sudo mkdir -rf /etc/nginx/conf.d
    sudo ln -s /etc/nginx/sites-available/* /etc/nginx/sites-enabled
}
function nginx-restart () {
    case "${OSTYPE}" in
        darwin*|linux*)
            sudo nginx -s stop
            sudo nginx
            sudo nginx -s status ;;
    esac
}
function nginx-status () {
    case "${OSTYPE}" in
        darwin*|linux*)
            sudo nginx -t
            sudo nginx -s status ;;
    esac
}
if ! type -p nginx > /dev/null; then get-nginx; fi
alias nr="nginx-restart"
alias np="ps aux | \grep -G 'nginx.*'"
alias ns="nginx-status"
alias nk="sudo killall nginx"


# 4. IntegratedDevelopmentEnvironment::Emacs
# ------------------------------------------
REQUIRED_EMACS_VERSION=24.5
# ### installation ###
function get-emacs () {
    case "${OSTYPE}" in
        cygwin) apt-cyg install emacs ;;
        freebsd*|darwin*|linux*)
            case "${DIST}" in
                Redhat|RedHat) sudo yum install -y ncurses-devel ;;
                Debian|Ubuntu)
                    sudo apt-get install -y build-essential libncurses-dev
                    sudo apt-get build-dep emacs ;;
            esac
            local current_pwd=`pwd`
            wget http://core.ring.gr.jp/pub/GNU/emacs/emacs-$REQUIRED_EMACS_VERSION.tar.gz;  wait
            tar zxf emacs-$REQUIRED_EMACS_VERSION.tar.gz;  wait
            cd emacs-$REQUIRED_EMACS_VERSION
            ./configure --with-xpm=no --with-gif=no --with-x-toolkit=no --with-tiff=no
            make
            yes | sudo make install;  wait
            cd $current_pwd; rm -fr emacs-$REQUIRED_EMACS_VERSION* ;;
    esac
}
if ! type -p emacs > /dev/null; then
   get-emacs
else
    _CURRENT_EMACS_VERSION=$(emacs --version | head -n 1 | sed 's/GNU Emacs //' | awk '$0 = substr($0, 1, index($0, ".") + 1)')
    if [[ $_REQUIRED_EMACS_VERSION > $_CURRENT_EMACS_VERSION ]]; then get-emacs; fi
fi
# ### aspell ###
function get-aspell () {
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
function get-mu () {
    case "${OSTYPE}" in
        freebsd*|darwin*|linux*)
            case "${DIST}" in
                Debian) ;;
                Ubuntu)
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
}
function mu-restart () {
    mv ~/Maildir ~/Maildir.org$(date +%y%m%d)
    mkdir -p ~/Maildir
    rm -fr ~/.mu
    rm -fr ~/.offlineimap
    offlineimap
    mu index --maildir=~/Maildir
    mu index --rebuild
    mu index
}
if ! type -p mu > /dev/null ; then get-mu ; fi


# 4. IntegratedDevelopmentEnvironment::Emacs::Ctags
# -------------------------------------------------
REQUIRED_EMACS_VERSION=5.8
function get-ctags () {
    local current_pwd=`pwd`
    wget http://prdownloads.sourceforge.net/ctags/ctags-$REQUIRED_EMACS_VERSION.tar.gz
    tar zxf ctags-$REQUIRED_EMACS_VERSION.tar.gz
    cd ctags-$REQUIRED_EMACS_VERSION
    ./configure --prefix=$HOME/.local
    make
    sudo make install
    cd $current_pwd
}
if ! type -p ctags > /dev/null ; then get-ctags ; fi
alias ctags=~/.local/bin/ctags


# 4. IntegratedDevelopmentEnvironment::ResourceManagement::Filesystem
# -------------------------------------------------------------------
# ### inotify ###
function get-inotify () {
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
function delete-log () {
    sudo find /home /var /usr -mtime +1 -a \( -name "*.pag" -o -name "*.dir" -o -name "*.log" \) -exec sudo rm {} \;
}
function sync-filesystem () {
    delete-log
    crontab -r
    echo '0 0 * * * sudo find /home /var /usr -mtime +1 -a \( -name "*.pag" -o -name "*.dir" -o -name "*.log" \) -exec sudo rm {} \;' | crontab
}


# 4. IntegratedDevelopmentEnvironment::ResourceManagement::Git
# ------------------------------------------------------------
REQUIRED_GIT_VERSION=1.9.4
# ### installation ###
function get-git () {
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
function get-git-flow () {
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
function get-hub () {
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
function get-gibo () {
    case "${OSTYPE}" in
        freebsd*) ;;
        darwin*|linux*)
            case $DIST in
                RedHat|Redhat) ;;
                Debian|Ubuntu) brew install gibo ;;
            esac
    esac
}
if ! type -p gibo > /dev/null ; then get-gibo ; fi
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
function ghclone () { git clone git@github.com:${1}.git }
function git-log () { git log ${1} --color --graph --pretty=format:"%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset" --abbrev-commit -- }
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
function git-trend () {
    g trend -n 10
    g trend -l ruby -n 5
    g trend -l elixir -n 5
    g trend -l erlang -n 5
    g trend -l JavaScript -n 5
    g trend -l php -n 3
}
function parse-git-dirty { git diff --no-ext-diff --quiet --exit-code &> /dev/null || echo "*" }
function parse-git-branch { git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e "s/* \(.*\)/(\1$(parse_git_dirty))/" }
function github-pull-repositories () {
    # # Setup Example:
    # function setup-git-pull-repositories () {
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
function get-puml () {
    case "${OSTYPE}" in
        freebsd*|darwin*) ;;
        linux*)
            case "${DIST}" in
                Redhat|RedHat) ;;
                Debian|Ubuntu)
                    if type -p npm > /dev/null; then npm install -g node-plantuml; fi ;;
            esac
    esac
}
function get-plantuml () {
    case "${OSTYPE}" in
        freebsd*|darwin*) ;;
        linux*)
            case "${DIST}" in
                Redhat|RedHat) ;;
                Debian|Ubuntu)
                    wget http://jaist.dl.sourceforge.net/project/plantuml/plantuml.8027.jar -O ~/.local/bin/plantuml.jar
                    alias plantuml='java -jar ~/.local/bin/plantuml.jar -tpng' ;;
            esac
    esac
}
function get-graphviz () {
    case "${OSTYPE}" in
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
function get-ab () {
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
# ### virtualbox ###
function vbm-scaleup () {
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


# 4. IntegratedDevelopmentEnvironment::SoftwareDeployment::Docker
# ---------------------------------------------------------------
# ### installation ###
function get-docker () {
    case "${OSTYPE}" in
        freebsd*|darwin*) ;;
        linux*)
            case "${DIST}" in
                Redhat|RedHat) sudo yum update && sudo yum install -y docker ;;
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
if ! type -p docker > /dev/null ; then get-docker ; fi
# ### alias ###
alias dc='docker commit $(docker ps -l -q)'
alias dd='docker rmi -f'
alias dda='docker rmi -f $(docker images -q)'
alias ddel='docker rmi -f'
alias ddela='docker rmi -f $(docker images -q)'
function dh () { docker history $1 | less -S }
alias dj='docker run -i -t'
alias dk='docker rm -f'
alias dka='docker rm -f $(docker ps -a -q)'
alias dkd='dka ; dda'
alias dkill='docker rm -f'
alias dkilla='docker rm -f $(docker ps -a -q)'
alias docker='sudo docker'
alias dl='docker images | less -S'
alias dls='docker images | less -S'
alias dp='docker ps -a | less -S'
alias dps='docker ps -a | less -S'
function dsshd () { docker run -t -d -p 5000:3000 -P $1 /usr/sbin/sshd -D }
alias dr='docker tag'
alias dv='docker images -viz'
function datach () { docker start $1 ; docker atach $1 }
function denv () { docker run --rm $1 env }
function dip () {
    CI=$(docker ps -l -q)
    if [ $1 ] ; then
	docker inspect --format {{.NetworkSettings.IPAddress}} $1
	docker inspect --format {{.NetworkSettings.Ports}} $1
    else
	docker inspect --format {{.NetworkSettings.IPAddress}} $CI
	docker inspect --format {{.NetworkSettings.Ports}} $CI
    fi
}
function dnsenter () {
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
function get-docker-compose () {
    case "${OSTYPE}" in
        freebsd*|darwin*) ;;
        linux*)
            case "${DIST}" in
                Redhat|RedHat) sudo pip install -U docker-compose ;;
                Debian) pip install -U docker-compose ;;
                Ubuntu)
                    case "${DIST_VERSION}" in
                        12.04) ;;
                        14.04) pip install -U docker-compose ;;
                    esac
            esac
    esac
}
function get-docker-machine () {
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


# 4. IntegratedDevelopmentEnvironment::SoftwareDeployment::Terraform
# ------------------------------------------------------------------
REQUIRED_TERRAFORM_VERSION=0.6.6
function get-terraform () {
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
function terraform-remote-config () {
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
setopt auto_cd         # auto change directory
setopt auto_pushd      # auto directory pushd that you can get dirs list by cd -[tab]
setopt correct         # command correct edition before each completion attempt
setopt list_packed     # compacked complete list display
setopt noautoremoveslash    # no remove postfix slash of command line
setopt nolistbeep      # no beep sound when complete list displayed


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
	precmd () {
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
    linux*|cygwin)
        alias ls='ls --color=auto'
        alias la="\ls -p -l -F -a --color=auto"
        alias lf="\ls -p -l -F --hide='.*' --color=auto"
        alias ll="\ls -p -F -a --color=auto"
        function lt () {
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
# ### srcreen for opening ssh ###
function ssh_screen () {
    eval server=\${$#}
    screen -t $server ssh "$@"
}
if [ x$TERM = xscreen ] ; then alias ssh=ssh_screen ; fi
# ### screen for status line ###
if [ "$TERM" = "screen" ]; then
    export LSCOLORS=gxfxcxdxbxegedabagacad
    export LS_COLORS='di=36;40:ln=35;40:so=32;40:pi=33;40:ex=31;40:bd=34;46:cd=34;43:su=0;41:sg=0;46:tw=0;42:ow=0;43:'
    # chpwd () { echo -n "_`dirs`\\" | ll }
    chpwd () { echo -n "_`dirs`\\" }
    preexec() {
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
function get-slackchat () {
    case "${OSTYPE}" in
        darwin*|linux*)
            wget https://github.com/vektorlab/slackcat/releases/download/v0.7/slackcat-0.7-linux-amd64 -O ~/.local/bin/slackchat
            chmod +x ~/.local/bin/slackchat
        ;;
    esac
}


# 5. Platform::Heroku
# -------------------
function get-heroku () {
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
function get-gcloud () {
    case "${OSTYPE}" in
        cygwin|darwin*|linux*)
            curl https://sdk.cloud.google.com | bash
            exec -l $SHELL ;;
    esac
}
function set-gcloud () {
    case "${OSTYPE}" in
        cygwin|darwin*|linux*)
            source '/home/vagrant/.local/google-cloud-sdk/completion.zsh.inc'
            source '/home/vagrant/.local/google-cloud-sdk/path.zsh.inc'
            source '/home/vagrant/.local/google-cloud-sdk/completion.zsh.inc' ;;
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
function get-kubectl () {
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
function get-aws () {
    if ! type -p pip > /dev/null ; then get-pip ; fi
    case "${OSTYPE}" in
        freebsd*) ;;
        darwin*|linux*) pip install -U awscli s3cmd ;;
    esac
}
if ! type -p aws > /dev/null ; then get-awscli ; fi


# 9. Other::Customized
# --------------------
# ### dove ###
function cd-dove () {
    dove_path=`which dove`
    dove_dir=`dirname $dove_path`
    cd $dove_dir/../$1
}
alias zd=cd-dove
# ### dotfiles ###
function get-dotfiles () {
    while getopts ":m:ih" opt ; do
        case $opt in
            "m") is_mu4e=true ;;
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
        case "${OSTYPE}" in (freebsd*|darwin*|linux*) cp -pr ~/.screenrc . ;; esac
        [ $is_mu4e ]    || git checkout -- .emacs.d/lisp/init-mu4e.el
        [ $is_init_el ] || git checkout -- .emacs.d/init.el
    fi
}
alias zg='get-dotfiles'
function put-dotfiles () {
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
    # main proc
    cp -pr .emacs.d/lisp/*       ~/.emacs.d/lisp/;   wait
    cp -pr .emacs.d/bin/*        ~/.emacs.d/bin/;    wait
    cp -pr .emacs.d/eshell/alias ~/.emacs.d/eshell/; wait
    cp -pr .emacs.d/init.el      ~/.emacs.d/;        wait
    cp -pr .offlineimap.py ~/;                       wait
    cp -pr .aspell.conf ~/;                          wait
    cp -pr .zshenv ~/;                               wait
    cp -pr .zshrc ~/;                                wait
    case "${OSTYPE}" in
        freebsd*|darwin*|linux*) cp -pr .screenrc ~/; wait ;;
    esac
    # post proc
    git checkout -- .emacs.d/lisp/init-mu4e.el
    cd $current_pwd
    exec -l zsh
}
alias zp=put-dotfiles
# ### other ###
function bkup () { cp -ipr $1 $1.org$(date +%y%m%d) }
function bkup-targz () { tar zcvf $2$(date +%y%m%d)_$1_$(date +%H).tar.gz $3$1 }
alias b='bkup'
alias bU='bkup-targz'
alias bin='~/bin'
alias c='/bin/cp -ipr'
alias d='/bin/rm -fr'
alias du="du -h"
alias df="df -h"
alias grep='egrep -ano'
alias egrep='\egrep -H -n'
function gresreg () {
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
    cygwin) alias ip="ps -flW" ;;
    linux*)
        case "${DIST}" in
            Redhat|RedHat) alias ip="ps aux" ;;
            Debian|Ubuntu) alias ip="ps aux" ;;
        esac
esac
alias it="date -R"
alias j='cd'
alias k='/bin/mkdir -p'
function kl () { kill -f $1 }
function chpwd(){ }
#function chpwd(){ ll }
function lower () {
    for i in "$@" ; do
        \mv -f $i $(echo $i | tr "[:upper:]" "[:lower:]")
    done
}
alias lw='lower'
alias m='/bin/mv'
alias mk="make config install distclean"
alias MK="make deinstall"
alias pwd='pwd -P'
function upper () {
    for i in * ; do
        \mv -f $i $(echo $i | tr "[:lower:]" "[:upper:]")
    done
}
alias up="upper"
alias r='/bin/mv'
alias re='e ~/.zshrc'
alias reb='cp -ip ~/.zshrc ~/.zshrc.org$(date +%y%m%d)'
function rename () {
    for i in *$1* ; do
        \mv -f $i `echo $i | sed -e s,$1,$2,g`
    done
}
alias rn="rename"
function rename-recursively () {
    find . -print | while read file ; do
        \mv -f $file ${file//$1/$2}
    done
}
alias rnr="rename-recursively"
function rr () { exec -l zsh }
function rename () {
    for i in *$1* ; do
        \mv -f $i    # (echo $i | sed -e s,$1,$2,g)
    done
}
alias rn="rename"
function rename-recursively () {
    find . -print | while read file ; do
        \mv -f $file ${file//$1/$2}
    done
}
alias rnr="rename-recursively"
alias s='/bin/ln -s'
alias scp='/usr/bin/scp -Cpr'
alias su="su -l"
alias u='tar zxvf'
alias U='tar zcvf $1.tar.gz $1'
alias uz='unzip'
alias v="cat"
function t () { \mv (.*~|.*.org*|*.org*|*.tar.gz|*.stackdump|*.tar.gz|*.asx|*.0|*.msi|*.wav|*.doc|*.pdf|$1) .old/ }
# ### other source file ###
if [ -f ~/.zshrc.mine ]; then source ~/.zshrc.mine; fi
