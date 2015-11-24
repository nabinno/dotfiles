# BASIC SETTINGS
# ===============
# os detect
# ---------
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
        ;;
esac


# environment variable
# --------------------
# export EDITOR=/usr/local/bin/vi
# export LANG=ja_JP.UTF-8
# export LANG=ja_JP.eucJP
REQUIRED_JAVA_VERSION=1.7.0
REQUIRED_PLAY_VERSION=2.2.3
REQUIRED_RUBY_VERSION=2.2.0
REQUIRED_PERL_VERSION=5.18
REQUIRED_PYTHON_VERSION=2.7.6
REQUIRED_MYSQL_VERSION=5.6
REQUIRED_GIT_VERSION=1.9.4
REQUIRED_MACPORT_VERSION=2.3.3
REQUIRED_TERRAFORM_VERSION=0.6.6
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
export RBENV_ROOT="$HOME/.local/rbenv"
export PATH=$HOME/bin:$HOME/local/bin:$PATH
export PATH="/opt/local/bin:$PATH"
export PATH="/opt/local/sbin:$PATH"
export PATH="$HOME/.jenv/bin:$PATH"
export PATH="$HOME/.local/bin:$PATH"
export PATH="$HOME/.local/rbenv/bin:$PATH"
export PATH="$HOME/.local/dove/bin:$PATH"
export PATH="$HOME/.parts/autoparts/bin:$PATH"
export PATH="$HOME/.parts/lib/node_modules/less/bin:$PATH"
export PATH="$HOME/.parts/packages/python2/$REQUIRED_PYTHON_VERSION/bin:$PATH"
export PATH="$HOME/.parts/packages/python2/$REQUIRED_PYTHON_VERSION/bin:$PATH"
export PATH="$HOME/.linuxbrew/bin:$PATH"
export PATH="$HOME/.cask/bin:$PATH"
export PATH="$HOME/.local/perl-$REQUIRED_PERL_VERSION/bin:$PATH"
export PS1="\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;36m\]\w\[\033[00m\]\$(parse_git_branch)\$ "


# locale
# ------
function set-locale () {
    case "${OSTYPE}" in
        linux*)
            case "${DIST}" in
                Redhat|RedHat)
                    sudo mv /etc/localtime{,.org}
                    sudo ln -s /usr/share/zoneinfo/Japan /etc/localtime
                    ;;
                Debian|Ubuntu)
                    sudo mv /etc/localtime{,.org}
                    sudo ln -s /usr/share/zoneinfo/Asia/Tokyo /etc/localtime
                    ;;
            esac
            ;;
    esac
}


# base installation
# -----------------
function get-base () {
    case "${OSTYPE}" in
        cygwin*)
            easy_install-2.7 \
                pip
            pip install -U \
                awscli \
                greenlet \
                eventlet \
                setuptools
            ;;
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
            ;;
        linux*)
            case "${DIST}" in
                Debian|Ubuntu)
                    sudo apt-get update
                    case "${DIST_VERSION=}" in
                        12.04)
                            sudo apt-get install -y python-software-properties
                            ;;
                        14.04)
                            sudo apt-get install -y \
                                 software-properties-common \
                                 python3-software-properties
                            ;;
                    esac
                    sudo add-apt-repository ppa:ondrej/mysql-$REQUIRED_MYSQL_VERSION
                    sudo add-apt-repository ppa:ondrej/php5
                    sudo add-apt-repository ppa:fcwu-tw/ppa
                    sudo add-apt-repository ppa:git-core/ppa
                    sudo apt-get update
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
                         libreadline5-dev \
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
                         zlib1g-dev
                    ;;
            esac
            ;;
    esac
}


# local
# -----
if [ ! -d ~/.local/bin ]; then mkdir -p ~/.local/bin; fi


# ruby
# ----
function get-ruby () {
    case "${OSTYPE}" in
        freebsd*)
            port install -y ruby
            ;;
        darwin*)
            brew install -y ruby
            ;;
        linux*)
            case "${DIST}" in
                Redhat|RedHat)
                    sudo yum install -y ruby
                    ;;
                Debian|Ubuntu)
                    sudo apt-get update -y && sudo apt-get -y ruby
                    ;;
                esac
            ;;
    esac
}
function get-rbenv () {
    case "${OSTYPE}" in
        freebsd*|darwin*|linux*)
            get-base
            rm -fr ~/.local/rbenv
            git clone git://github.com/sstephenson/rbenv.git ~/.local/rbenv
            mkdir -p ~/.local/rbenv/shims ~/.local/rbenv/versions ~/.local/rbenv/plugins
            git clone git://github.com/sstephenson/ruby-build.git ~/.local/rbenv/plugins/ruby-build
            git clone https://github.com/sstephenson/rbenv-gem-rehash.git ~/.local/rbenv/plugins/rbenv-gem-rehash
            eval "$(rbenv init -)"
            exec $SHELL -l
            rbenv install $REQUIRED_RUBY_VERSION
            rbenv rehash
            rbenv global $REQUIRED_RUBY_VERSION
            gem install \
                bundler \
                compass \
                haml \
                html2slim \
                rails \
                rubygems-bundler \
                sidekiq \
                unicorn
            eval "$(rbenv init -)"
            ;;
    esac
}
if ! type -p ruby > /dev/null;  then get-ruby;  fi
if ! type -p rbenv > /dev/null; then
    get-rbenv
else
    eval "$(rbenv init -)"
fi


# autoparts
# ---------
case "${OSTYPE}" in
    linux*)
        case "${DIST}" in
            Debian|Ubuntu)
                if ! type -p parts > /dev/null; then
                    install-base
                    ruby -e "$(curl -fsSL https://raw.github.com/nitrous-io/autoparts/master/setup.rb)"
                    eval "$(parts env)"
                    exec $SHELL -l
                    parts install \
                          chruby \
                          ctags \
                          elixir \
                          erlang \
                          go \
                          heroku_toolbelt \
                          maven \
                          nodejs \
                          phantomjs \
                          pip \
                          the_silver_searcher \
                          tree
                    npm install -g \
                        bower \
                        grunt-cli \
                        gulp \
                        http-server \
                        html2jade \
                        less \
                        node-plantuml \
                        npm2dot \
                        phantomjs \
                        requirejs \
                        term
                    gem install \
                        bundler \
                        compass \
                        haml \
                        html2slim \
                        rails \
                        rubygems-bundler \
                        sidekiq \
                        unicorn
                    pip install -U \
                        awscli \
                        docker-compose
                fi
                eval "$(parts env)"
                if ! type -p npm > /dev/null; then
                    parts install nodejs
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
                        requirejs
                        term
                fi
                if ! type -p gem > /dev/null; then
                    parts install gem
                    gem install \
                        bundler \
                        compass \
                        haml \
                        html2slim \
                        rails \
                        rubygems-bundler \
                        sidekiq \
                        unicorn
                fi
                if ! type -p pip > /dev/null; then
                    parts install pip
                    pip install -U \
                        awscli \
                        docker-compose \
                        ipython \
                        pandas \
                        pulp \
                        simpy \
                        boto
                fi
	        ;;
        esac
esac


# homebrew/linuxbrew
# ------------------
function get-brew () {
    case "${OSTYPE}" in
        darwin*)
            brew install \
                 chruby \
                 ctags \
                 elixir \
                 elixir-build \
                 erlang \
                 go \
                 jq \
                 lua \
                 maven \
                 memcached \
                 mruby \
                 mysql \
                 nodejs \
                 redis \
                 rust \
                 scalaenv \
                 the_silver_searcher \
                 tmux \
                 tree \
                 vert.x
            npm install -g \
                bower \
                grunt-cli \
                gulp \
                http-server \
                html2jade \
                less \
                node-plantuml \
                npm2dot \
                phantomjs \
                requirejs \
                term
            gem install \
                bundler \
                compass \
                haml \
                html2slim \
                rails \
                rubygems-bundler \
                sidekiq \
                unicorn
            ;;
        linux*)
                ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/linuxbrew/go/install)"
                exec $SHELL -l
                case "${DIST}" in
                    Redhat|RedHat)
                        brew install \
                             chruby \
                             ctags \
                             elixir \
                             elixir-build \
                             erlang \
                             go \
                             heroku_toolbelt \
                             jq \
                             lua \
                             maven \
                             mruby \
                             rust \
                             scalaenv \
                             the_silver_searcher \
                             tmux \
                             tree \
                             vert.x
                        ;;
                    Debian|Ubuntu)
                        brew install \
                             jq \
                             scalaenv \
                             vert.x
                        ;;
                esac
            ;;
    esac
}
if ! type -p brew > /dev/null; then
    get-brew
fi


# java
# ----
export PLAY_HOME=/usr/local/play-$REQUIRED_PLAY_VERSION
export PATH="$PLAY_HOME:$PATH"
case "${OSTYPE}" in
    freebsd*|darwin*|linux*)
        if [ ! -d ~/.jenv ]; then
            git clone https://github.com/gcuisinier/jenv.git ~/.jenv
            eval "$(jenv init -)"
        fi
        ;;
esac
function get-java () {
    case "${OSTYPE}" in
        freebsd*|darwin*)
            sudo pkg install -y openjdk
            ;;
        linux*)
            case "${DIST}" in
                Redhat|RedHat)
                    sudo yum install -y java-$REQUIRED_JAVA_VERSION-openjdk
                    sudo yum install -y java-$REQUIRED_JAVA_VERSION-openjdk-devel
                    ;;
                Debian|Ubuntu)
                    sudo apt-get update
                    sudo apt-get install -y openjdk-7-jdk
                    ;;
            esac
            ;;
    esac
}
function set-javahome () {
    case "${OSTYPE}" in
        freebsd*|darwin*)
            ;;
        linux*)
            case "${DIST}" in
                Redhat|RedHat)
                    export JAVA_HOME=/usr/lib/jvm/java-$REQUIRED_JAVA_VERSION
                    ;;
                Debian|Ubuntu)
                    export JAVA_HOME=/usr/lib/jvm/default-java
                    ;;
            esac
            ;;
    esac
}
function get-play () {
    case "${OSTYPE}" in
        freebsd*|darwin*|linux*)
            wget http://downloads.typesafe.com/play/$REQUIRED_PLAY_VERSION/play-$REQUIRED_PLAY_VERSION.zip
            unzip play-$REQUIRED_PLAY_VERSION.zip
            mv play-$REQUIRED_PLAY_VERSION ~/.local/
            rm -fr play-$REQUIRED_PLAY_VERSION.zip
            ;;
    esac
}
function get-sbt () {
    case "${OSTYPE}" in
        freebsd*|darwin*)
            sudo port install sbt
            ;;
        linux*)
            case "${DIST}" in
                Redhat|RedHat)
                    curl https://bintray.com/sbt/rpm/rpm | sudo tee /etc/yum.repos.d/bintray-sbt-rpm.repo
                    sudo yum install -y sbt
                    ;;
                Debian|Ubuntu)
                    echo "deb https://dl.bintray.com/sbt/debian /" | sudo tee -a /etc/apt/sources.list.d/sbt.list
                    sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv 642AC823
                    sudo apt-get update
                    sudo apt-get install -y sbt
                    ;;
            esac
            ;;
    esac
}
if ! type -p java > /dev/null; then
    get-java
    set-javahome
else
    REQUIRED_JAVA_VERSION=$(echo $REQUIRED_JAVA_VERSION | sed 's/\(.*\..*\)\..*/\1/')
    CURRENT_JAVA_VERSION=$(java -version 2>&1 | head -n 1 | cut -d\" -f 2 | sed 's/\(.*\..*\)\..*/\1/')
    if [[ $REQUIRED_JAVA_VERSION > $CURRENT_JAVA_VERSION ]]; then get-java; fi
    set-javahome
fi
if [ -d ~/.local/play-$REQUIRED_PLAY_VERSION ]; then
    get-play
    get-sbt
fi


# php
# ---
function get-php () {
    case "${OSTYPE}" in
        freebsd*|darwin*|linux*)
            case "${DIST}" in
                Debian|Ubuntu)
                    sudo apt-get update
                    sudo apt-get install -y php5-common php5-cli php5-fpm
                    ;;
            esac
            ;;
    esac
}
function get-fastcgi () {
    case "${OSTYPE}" in
        linux*)
            case "${DIST}" in
                Redhat|RedHat)
                    sudo yum install -y php-fpm
                    chkconfig php-fpm on
                ;;
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
                            sudo update-rc.d php-fastcgi defaults
                            ;;
                        14.04)
                            sudo apt-get update -y
                            sudo apt-get install -y \
                                 nginx \
                                 php5-cli \
                                 spawn-fcgi \
                                 psmisc
                            ;;
                    esac
            esac
            ;;
    esac
}
function php-fastcgid () {
    case "${OSTYPE}" in
        darwin*)
            ;;
        linux*)
            case "${DIST}" in
                Redhat|RedHat)
                    sudo /etc/init.d/php-fpm $1
                ;;
                Ubuntu)
                    case "${DIST_VERSION}" in
                        14.04)
                            sudo service php5-fpm $1
                            ;;
                        12.04)
                            sudo /etc/init.d/php-fastcgi $1
                            ;;
                    esac
                    ;;
            esac
            ;;
    esac
}
function fastcgi-restart () {
    sudo killall php-fpm php-fastcgi php-fastcgid $1; wait
    case "${OSTYPE}" in
        darwin*)
            ;;
        linux*)
            php-fastcgid start
            php-fastcgid status
            ;;
    esac
}
if ! type -p php > /dev/null; then get-php; fi
if [ ! -f /etc/init.d/php-fastcgi ] && [ ! -f /etc/init.d/php-fpm ] && [ ! -f /etc/init.d/php5-fpm ] ; then
    get-fastcgi
fi
alias fr="fastcgi-restart"
alias fp="ps aux | \grep -G 'php.*'"
alias fs="php-fastcgid status"
alias fk="php-fastcgid stop"


# python
# ------
function get-python () {
    case "${OSTYPE}" in
        darwin*)
            sudo easy_install pip
            sudo pip install -U \
                 awscli \
                 docker-compose
            ;;
        linux*)
            case "${DIST}" in
                Redhat|RedHat)
                    sudo easy_install pip
                    sudo pip install -U \
                         awscli \
                         docker-compose
                    ;;
            esac
            ;;
    esac
}
if ! type -p pip > /dev/null; then
    get-python
fi


# perl
# ----
function get-perl () {
    case "${OSTYPE}" in
        freebsd*|darwin*|linux*)
            git clone https://github.com/tagomoris/xbuild.git ~/.local/xbuild
            ~/.local/xbuild/perl-install $REQUIRED_PERL_VERSION.2 ~/.local/perl-$REQUIRED_PERL_VERSION
            ;;
    esac
}
function get-plagger () {
    cpanm -fi \
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
        Plagger
}
function cpanmodulelist () {
    perl -e "print \"@INC\"" | find -name "*.pm" -print 
}
function cpanmoduleversion () {
    perl -M$1 -le "print \$$1::VERSION" 
}
function cpan-uninstall () {
    perl -MConfig -MExtUtils::Install -e '($FULLEXT=shift)=~s{-}{/}g;uninstall "$Config{sitearchexp}/auto/$FULLEXT/.packlist",1'
}
if [ ! -d ~/.local/xbuild ]; then
    get-perl
fi
alias cpanm='~/.local/perl-5.18/bin/cpanm'
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


# javascript
# ----------
export node='NODE_NO_READLINE=1 node'


# mysql
# -----
function get-mysql () {
    case "${OSTYPE}" in
        darwin*)
            brew install mysql
            ;;
        linux*)
            case "${DIST}" in
                Redhat|RedHat)
                ;;
                Debian)
                ;;
                Ubuntu)
                        sudo apt-get -y remove mysql-server
                        sudo apt-get -y autoremove
                        sudo apt-get -y install software-properties-common
                        sudo add-apt-repository -y ppa:ondrej/mysql-$REQUIRED_MYSQL_VERSION
                        sudo apt-get update
                        sudo apt-get -y install mysql-server
                    ;;
            esac
    esac
}
if ! type -p mysql > /dev/null; then
    get-mysql
fi
function my-restart () {
    sudo killall mysqld $1; wait
    case "${OSTYPE}" in
        darwin*)
            brew install mysql
            ;;
        linux*)
            case "${DIST}" in
                Redhat|RedHat)
                    sudo service mysql start
                    sudo service mysql status
                    ;;
                Debian|Ubuntu)
                    sudo /etc/init.d/mysql start
                    sudo /etc/init.d/mysql status
                    ;;
            esac
            ;;
    esac
}
alias mr="my-restart"
alias mp="ps aux | \grep -G 'mysql.*'"
alias ms="sudo /etc/init.d/mysql status"
alias mk="sudo killall mysqld"


# nginx
# -----
funtion get-nginx () {
    case "${OSTYPE}" in
        darwin*)
            brew tap homebrew/nginx
            brew install nginx-full --with-status --with-upload-module
            brew unlink nginx
            brew link nginx-full
        ;;
        linux*)
            case "${DIST}" in
                Redhat|RedHat)
                    sudo yum install -y \
                         pcre \
                         pcre-devel \
                         zlib \
                         zlib-devel \
                         openssl \
                         openssl-devel \
                         gcc
                    sudo useradd -s/sbin/nologin -d/usr/local/nginx -M nginx
                    sudo rpm -ivh http://nginx.org/packages/centos/6/noarch/RPMS/nginx-release-centos-6-0.el6.ngx.noarch.rpm
                    sudo yum install nginx --disablerepo=amzn-main -y
                    sudo chkconfig nginx on
                    echo "priority=1" >> /etc/yum.repos.d/nginx.repo
                ;;
            esac
            ;;
    esac
}
function nginx-restart () {
    case "${OSTYPE}" in
        darwin*)
            sudo nginx -s reload
            sudo nginx -s status
            ;;
        linux*)
            case "${DIST}" in
                Redhat|RedHat)
                    sudo service nginx restart
                    sudo service nginx status
                ;;
                Ubuntu)
                    case "${DIST_VERSION=}" in
                        12.04)
                            sudo /etc/init.d/nginx restart
                            sudo /etc/init.d/nginx status
                            ;;
                        14.04)
                            sudo service nginx restart
                            sudo service nginx status
                    esac
                    ;;
            esac
            ;;
    esac
}
if ! type -p nginx > /dev/null; then get-nginx; fi
alias nr="nginx-restart"
alias np="ps aux | \grep -G 'nginx.*'"
alias ns="sudo /etc/init.d/nginx status"
alias nk="sudo killall nginx"


# git
# ---
function get-git () {
    case "${OSTYPE}" in
        freebsd*|darwin*)
            port install git-flow
        ;;
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
                    rm -fr git-${REQUIRED_GIT_VERSION}
                ;;
                Debian)
                ;;
                Ubuntu)
                    case "${DIST_VERSION=}" in
                        12.04)
                            sudo apt-get install -y \
                                 python-software-properties
                            ;;
                        14.04)
                            sudo apt-get install -y \
                                 software-properties-common \
                                 python3-software-properties
                            ;;
                    esac
                    sudo add-apt-repository ppa:git-core/ppa
                    sudo apt-get update
                    sudo apt-get install -y git git-flow
                    ;;
            esac
            ;;
    esac
}
if ! type -p git > /dev/null; then
    get-git
else
    REQUIRED_GIT_VERSION_NUM=$(echo $REQUIRED_GIT_VERSION | sed 's/\(.*\..*\)\..*/\1/')
    CURRENT_GIT_VERSION=$(git --version 2>&1 | cut -d\  -f 3 | sed 's/\(.*\..*\)\..*/\1/')
    if [[ $REQUIRED_GIT_VERSION_NUM > $CURRENT_GIT_VERSION ]]; then get-git; fi
fi
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
function ghclone () { git clone https://${2}github.com/${1}.git }
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
function parse_git_dirty {
    git diff --no-ext-diff --quiet --exit-code &> /dev/null || echo "*"
}
function parse_git_branch {
    git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e "s/* \(.*\)/(\1$(parse_git_dirty))/"
}


# emacs
# -----
REQUIRED_EMACS_VERSION=24.5
function get-emacs () {
    case "${OSTYPE}" in
        freebsd*|darwin*|linux*)
            case "${DIST}" in
                Redhat|RedHat)
                    sudo yum install -y ncurses-devel
                    ;;
                Debian|Ubuntu)
                    sudo apt-get install -y build-essential libncurses-dev
                    sudo apt-get build-dep emacs
                    ;;
            esac
            current_pwd=`pwd`
            wget http://core.ring.gr.jp/pub/GNU/emacs/emacs-$REQUIRED_EMACS_VERSION.tar.gz;  wait
            tar zxf emacs-$REQUIRED_EMACS_VERSION.tar.gz;  wait
            cd emacs-$REQUIRED_EMACS_VERSION
            ./configure --with-xpm=no --with-gif=no --with-x-toolkit=no --with-tiff=no
            make
            yes | sudo make install;  wait
            cd $current_pwd; rm -fr emacs-$REQUIRED_EMACS_VERSION*
            ;;
    esac
}
function get-mu () {
    case "${OSTYPE}" in
        freebsd*|darwin*|linux*)
            case "${DIST}" in
                Debian)
                ;;
                Ubuntu)
                    case "${DIST_VERSION}" in
                        12.04)
                            sudo apt-get install -y \
                                 libgmime-2.6-dev \
                                 libxapian-dev \
                                 gnutls-bin \
                                 guile-2.0-dev \
                                 html2text \
                                 xdg-utils \
                                 offlineimap \
                                 git clone https://github.com/djcb/mu
                            cd mu
                            sudo autoreconf -i
                            ./configure && make
                            sudo make install
                            cd .. && rm -fr mu
                            ln -s /usr/local/share/emacs/site-lisp/mu4e $HOME/.emacs.d/site-lisp/
                            ;;
                    esac
            esac
            ;;
    esac
}
if ! type -p emacs > /dev/null; then
    get-emacs
else
    CURRENT_EMACS_VERSION=$(emacs --version | head -n 1 | sed 's/GNU Emacs //' | awk '$0 = substr($0, 1, index($0, ".") + 1)')
    if [[ $REQUIRED_EMACS_VERSION > $CURRENT_EMACS_VERSION ]]; then get-emacs; fi
fi
if ! type -p mu > /dev/null; then
    get-mu
fi

# default shell
# -------------
autoload colors
colors
case ${UID} in
    0)
	PROMPT="%B%{${fg[red]}%}%/#%{${reset_color}%}%b "
	PROMPT2="%B%{${fg[red]}%}%_#%{${reset_color}%}%b "
	SPROMPT="%B%{${fg[red]}%}%r is correct? [n,y,a,e]:%{${reset_color}%}%b "
	[ -n "${REMOTEHOST}${SSH_CONNECTION}" ] &&
        PROMPT="%{${fg[cyan]}%}$(echo ${HOST%%.*} | tr '[a-z]' '[A-Z]') ${PROMPT}"
	;;
    *)
	PROMPT="%{${fg[red]}%}%/%%%{${reset_color}%} "
	PROMPT2="%{${fg[red]}%}%_%%%{${reset_color}%} "
	SPROMPT="%{${fg[red]}%}%r is correct? [n,y,a,e]:%{${reset_color}%} "
	[ -n "${REMOTEHOST}${SSH_CONNECTION}" ] &&
        PROMPT="%{${fg[cyan]}%}$(echo ${HOST%%.*} | tr '[a-z]' '[A-Z]') ${PROMPT}"
	;;
esac

setopt auto_cd         # auto change directory
setopt auto_pushd      # auto directory pushd that you can get dirs list by cd -[tab]
setopt correct         # command correct edition before each completion attempt
setopt list_packed     # compacked complete list display
setopt noautoremoveslash    # no remove postfix slash of command line
setopt nolistbeep      # no beep sound when complete list displayed


# key bind
# --------
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


# zsh editor
# ----------
# autoload zed


# zmv
# ---
autoload -Uz zmv


# prediction
# ----------
# autoload predict-on
# predict-off


# terminal
# --------
unset LSCOLORS
zstyle ':completion:*' use-cache true
case "${TERM}" in
    xterm|screen.xterm)
	export TERM=xterm-color
	;;
    kterm)
	export TERM=kterm-color
        # set BackSpace control character
	stty erase
	;;
    cons25)
	unset LANG
	export LSCOLORS=ExFxCxdxBxegedabagacad
	export LS_COLORS='di=01;34:ln=01;35:so=01;32:ex=01;31:bd=46;34:cd=43;34:su=41;30:sg=46;30:tw=42;30:ow=43;30'
	zstyle ':completion:*' list-colors \
	    'di=;34;1' 'ln=;35;1' 'so=;32;1' 'ex=31;1' 'bd=46;34' 'cd=43;34'
	;;
    indumb|emacs)
	# PROMPT="%n@%~%(!.#.$)"
	PROMPT="%{${fg[red]}%}%/%%%{${reset_color}%} "
        PROMPT="%{${fg[cyan]}%}$(echo ${HOST%%.*} | tr '[a-z]' '[A-Z]') ${PROMPT}"
	RPROMPT=""
	unsetopt zle
	;;
esac

# ### set terminal title including current directory ###
case "${TERM}" in
    kterm*|xterm*)
	precmd() {
	    echo -ne "\033]0;${USER}@${HOST%%.*}:${PWD}\007"
	}
	export LSCOLORS=gxfxcxdxbxegedabagacad
	export LS_COLORS='di=36;40:ln=35;40:so=32;40:pi=33;40:ex=31;40:bd=34;46:cd=34;43:su=0;41:sg=0;46:tw=0;42:ow=0;43:'
	zstyle ':completion:*' list-colors \
	    'di=36' 'ln=35' 'so=32' 'ex=31' 'bd=46;34' 'cd=43;34'
	;;
esac


# screen
# ------
# ### srcreen for opening ssh ###
function ssh_screen(){
    eval server=\${$#}
    screen -t $server ssh "$@"
}
if [ x$TERM = xscreen ]; then
    alias ssh=ssh_screen
fi

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
	local -a cmd; cmd=(${(z)2})
	case $cmd[1] in
	    fg)
		if (( $#cmd == 1 )); then
		    cmd=(builtin jobs -l %+)
		else
		    cmd=(builtin jobs -l $cmd[2])
		fi
		;;
	    %*)
		cmd=(builtin jobs -l $cmd[1])
		;;
	    cd)
		if (( $#cmd == 2)); then
		    cmd[1]=$cmd[2]
		fi
		;;
	    *)
		echo -n "k$cmd[1]:t\\"
		return
		;;
	esac
	local -A jt; jt=(${(kv)jobtexts})
	$cmd >>(read num rest
	    cmd=(${(z)${(e):-\$jt$num}})
	    echo -n "k$cmd[1]:t\\") 2>/dev/null
    }
    chpwd
fi


# plantuml
# --------
case "${OSTYPE}" in
    freebsd*|darwin*)
    ;;
    linux*)
        case "${DIST}" in
            Redhat|RedHat)
            ;;
            Debian|Ubuntu)
                if ! type -p puml > /dev/null; then npm install -g node-plantuml; fi
                if [ ! -f ~/.local/bin/plantuml.jar ] ; then
                    wget http://jaist.dl.sourceforge.net/project/plantuml/plantuml.8027.jar -O ~/.local/bin/plantuml.jar
                    alias plantuml='java -jar ~/.local/bin/plantuml.jar -tpng'
                fi
                ;;
        esac
        ;;
esac
if ! type -p dot > /dev/null; then
    case "${OSTYPE}" in
        freebsd*|darwin*)
            brew install graphviz
            ;;
        linux*)
            case "${DIST}" in
                Redhat|RedHat)
                    sudo yum install -y graphviz
                    ;;
                Debian|Ubuntu)
                    sudo apt-get update
                    sudo apt-get install -y graphviz
                    ;;
            esac
            ;;
    esac
fi


# benchmark
# ---------
if ! type -p ab > /dev/null; then
    case "${OSTYPE}" in
        freebsd*|darwin*)
            ;;
        linux*)
            case "${DIST}" in
                Redhat|RedHat)
                    sudo yum install -y httpd-tools
                    ;;
                Debian|Ubuntu)
                    sudo apt-get install -y apache2-utils
                    ;;
            esac
            ;;
    esac
fi


# vagrannt
# --------
alias vu='vagrant up'
alias vp='vagrant global-status'
alias vk='vagrant destroy --force'
alias vl='vagrant box list'
alias vd='vagrant box remove'
alias vsh='vagrant ssh'
alias vshconfig='vagrant ssh-config'


# docker
# ------
# ### installation ###
if ! type -p docker > /dev/null; then
    case "${OSTYPE}" in
        freebsd*|darwin*)
        ;;
        linux*)
            case "${DIST}" in
                Redhat|RedHat)
                    sudo yum install -y docker
                    ;;
                Debian)
                    sudo apt-get update; sudo apt-get install -y docker.io
	            ;;
                Ubuntu)
                    sudo apt-key adv --keyserver hkp://pgp.mit.edu:80 --recv-keys 58118E89F3A912897C070ADBF76221572C52609D
                    if [ -f /etc/apt/sources.list.d/docker.list ]; then
                        sudo rm /etc/apt/sources.list.d/docker.list
                        sudo touch /etc/apt/sources.list.d/docker.list
                    fi
                    case "${DIST_VERSION}" in
                        12.04)
                            sudo sh -c 'echo "deb https://apt.dockerproject.org/repo ubuntu-precise main" >> /etc/apt/sources.list.d/docker.list'
			    ;;
                        14.04)
                            sudo sh -c 'echo "deb https://apt.dockerproject.org/repo ubuntu-trusty main" >> /etc/apt/sources.list.d/docker.list'
			    ;;
		    esac
                    sudo apt-get update
                    sudo apt-get purge lxc-docker*
                    sudo apt-cache policy docker-engine
		    sudo apt-get update
                    sudo apt-get install -y docker-engine
		    ;;
            esac
            ;;
    esac
fi

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
    if [ $1 ]; then
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
case "${OSTYPE}" in
    freebsd*|darwin*)
    ;;
    linux*)
        case "${DIST}" in
            Redhat|RedHat)
                if ! type -p docker-compose > /dev/null; then sudo pip install -U docker-compose; fi
            ;;
            Debian|Ubuntu)
                if ! type -p docker-compose > /dev/null; then pip install -U docker-compose; fi
                if ! type -p docker-machine > /dev/null; then
                    wget https://github.com/docker/machine/releases/download/v0.1.0/docker-machine_linux-386 -O ~/.local/bin/docker-machine
                    chmod +x ~/.local/bin/docker-machine
                fi
	        ;;
        esac
        ;;
esac


# terraform
# ---------
function get-terraform () {
    current_pwd=`pwd`
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
alias trc="terraform-remote-config"
alias tr="terraform remote"
alias ts="terraform show"
alias tp="terraform plan"
alias ta="terraform apply"
alias td="terraform deploy"



# ALIAS
# =====
zmodload -i zsh/mathfunc
setopt complete_aliases     # aliased ls needs if file/dir completions work


# extentions
# ----------
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


# other aliases
# -------------
alias today='date +%Y-%m-%d'
alias where="command -v"
case "${OSTYPE}" in
    freebsd*|darwin*)
	alias ls="ls -G -w"
        alias lf="\ls -p -l -F -G"
        alias ll="\ls -p -F -a -G"
        alias la="\ls -p -l -F -a -G"
	;;
    linux*|cygwin*)
        alias ls='ls --color=auto'
        alias la="\ls -p -l -F -a --color=auto"
        alias lf="\ls -p -l -F --hide='.*' --color=auto"
        alias ll="\ls -p -F -a --color=auto"
        function lt () {
            \ls -R $1 | \grep ":$" | \sed -e 's/:$//' -e 's/[^-][^\/]*\//--/g' -e 's/^/   /' -e 's/-/|/'
        }
	;;
esac
case "${OSTYPE}" in
    darwin*)
	alias updateports="sudo port selfupdate; sudo port outdated"
	alias portupgrade="sudo port upgrade installed"
	;;
    freebsd*)
	case ${UID} in
	    0)
		updateports()
		{
		    if [ -f /usr/ports/.portsnap.INDEX ]
		    then
			portsnap fetch update
		    else
			portsnap fetch extract update
		    fi
		    (cd /usr/ports/; make index)
		    portversion -v -l \<
		}
		alias appsupgrade='pkgdb -F && BATCH=YES NO_CHECKSUM=YES portupgrade -a'
		;;
	esac
	;;
esac


# customized
# ----------
# ### dove ###
function cd-dove () {
    dove_path=`which dove`
    dove_dir=`dirname $dove_path`
    cd $dove_dir/../$1
}
alias zd=cd-dove

# ### dotfiles ###
function get-dotfiles () {
    # pre proc
    cd ~/; wait
    if [ ! -d ~/.local/dotfiles ]; then
        mkdir -p ~/.local
        sh -c "$(curl -fsSL https://raw.github.com/nabinno/dotfiles/master/install)"; wait
    fi
    cd ~/.local/dotfiles; wait
    git checkout -- .;  wait
    git checkout -b develop origin/develop;  wait
    git pull origin develop;  wait
    # main proc
    rm -rf                         .emacs.d/lisp/*;    wait
    cp -pr ~/.emacs.d/lisp/*       .emacs.d/lisp/;     wait
    cp -pr ~/.emacs.d/bin/*        .emacs.d/bin/;      wait
    cp -pr ~/.emacs.d/eshell/alias .emacs.d/eshell/;   wait
    cp -pr ~/.emacs.d/init.el      .emacs.d/;          wait
    cp -pr ~/.offlineimap.py .
    cp -pr ~/.zshrc .
    case "${OSTYPE}" in
        freebsd*|darwin*|linux*)
            cp -pr ~/.screenrc .
            ;;
    esac
    # post proc
    git checkout -- .emacs.d/lisp/init-mu4e.el
    cp -pr .tmp/.* .
}
alias zg=get-dotfiles
function put-dotfiles () {
    # pre proc
    current_pwd=`pwd`
    cd ~/
    if [ ! -d ~/.local/dotfiles ]; then
        mkdir -p ~/.local
        sh -c "$(curl -fsSL https://raw.github.com/nabinno/dotfiles/master/install)"; wait
    fi
    cd ~/.local/dotfiles; wait
    git checkout -- .;  wait
    git checkout -b develop origin/develop;  wait
    git pull origin develop;  wait
    rm -rf .emacs.d/lisp/init-mu4e.el
    # main proc
    cp -pr .emacs.d/lisp/*       ~/.emacs.d/lisp/;   wait
    cp -pr .emacs.d/bin/*        ~/.emacs.d/bin/;    wait
    cp -pr .emacs.d/eshell/alias ~/.emacs.d/eshell/; wait
    cp -pr .emacs.d/init.el      ~/.emacs.d/;        wait
    cp -pr .offlineimap.py ~/;                       wait
    cp -pr .zshrc ~/;                                wait
    case "${OSTYPE}" in
        freebsd*|darwin*|linux*)
            cp -pr .screenrc ~/; wait
            ;;
    esac
    # post proc
    git checkout -- .emacs.d/lisp/init-mu4e.el
    cd $current_pwd
    source ~/.zshrc
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
alias e='emacsclient -c t'
alias ij='jobs -l'
alias ic='cat /proc/cpuinfo'
alias in='netstat -a -n | more'
alias im='cat /proc/meminfo'
case "${OSTYPE}" in
    freebsd*|darwin*)
        alias ip="ps aux"
        ;;
    cygwin*)
        alias ip="ps -flW"
        ;;
    linux*)
        case "${DIST}" in
            Redhat|RedHat)
                alias ip="ps aux"
                ;;
            Debian|Ubuntu)
                alias ip="ps aux"
                ;;
        esac
        ;;
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
alias rr='source ~/.zshrc'
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
