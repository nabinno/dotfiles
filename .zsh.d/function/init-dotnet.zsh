export REQUIRED_DOTNETFRAMEWORK_VERSION=4.0.30319
export REQUIRED_MONO_VERSION=4.0.4.1
export REQUIRED_NUNIT_VERSION=3.2.1
export DOTNET_HOME=$HOME/.local/dotnet
export PATH=$PATH:$DOTNET_HOME
export PATH="$HOME/.local/NUnit/bin:$PATH"
case "${OSTYPE}" in
  msys)
    export PATH="/C/Windows/Microsoft.NET/Framework64/v${REQUIRED_DOTNETFRAMEWORK_VERSION}:$PATH"
    export PATH=/C/Program~2/Mono/bin:$PATH
    ;;
  cygwin)
    export PATH="/cygdrive/C/Windows/Microsoft.NET/Framework64/v${REQUIRED_DOTNETFRAMEWORK_VERSION}:$PATH"
    export PATH=/cygdrive/C/Program~2/Mono/bin:$PATH
    ;;
esac

# ----------------------------------------------------------------------
# A. Compiler        - Mono
# B. Task Runner     - Dotnet CLI
# C. Package Manager - NuGet
# D. Scaffolding     - Yeoman / Grunt-init
# E. Complition      - OmniSharp
# F. Test Runner     - NUnit
# ### A. Compiler ###
get-mono() {
  case "${OSTYPE}" in
    msys | cygwin) choco install mono ;;
    freebsd* | darwin*) ;;
    linux*)
      case $DIST in
        Redhat* | RedHat*)
          sudo yum install -y yum-utils
          sudo rpm --import "http://keyserver.ubuntu.com/pks/lookup?op=get&search=0x3FA7E0328081BFF6A14DA29AA6A19B38D3D831EF"
          sudo yum-config-manager --add-repo http://download.mono-project.com/repo/centos/
          sudo yum install -y \
            mono \
            mono-complete
          ;;
        Debian | Ubuntu*)
          case $DIST_VERSION in
            14.04 | 16.04)
              sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 3FA7E0328081BFF6A14DA29AA6A19B38D3D831EF
              echo "deb http://download.mono-project.com/repo/debian wheezy main" | sudo tee /etc/apt/sources.list.d/mono-xamarin.list
              sudo apt-get update
              sudo apt-get install -y \
                mono \
                mono-complete
              ;;
          esac
          ;;
      esac
      ;;
  esac
}

# ----------------------------------------------------------------------
# ### B. Task Runner ###
get-dotnetcli() {
  case "${OSTYPE}" in
    msys)
      wget https://dotnetcli.blob.core.windows.net/dotnet/beta/Binaries/Latest/dotnet-dev-rhel-x64.latest.tar.gz
      pacman -S libicu libuuid libcurl openssl libunwind
      mkdir ~/.local/dotnet
      tar xf dotnet-dev-rhel-x64.latest.tar.gz -C ~/.local/dotnet --verbose
      rm -fr dotnet-dev-rhel-x64.latest.tar.gz
      ;;
    cygwin)
      # ### dotnet ###
      wget https://dotnetcli.blob.core.windows.net/dotnet/beta/Binaries/Latest/dotnet-dev-rhel-x64.latest.tar.gz
      apt-cyg install -y libicu libuuid libcurl openssl libunwind
      mkdir ~/.local/dotnet
      tar xf dotnet-dev-rhel-x64.latest.tar.gz -C ~/.local/dotnet --verbose
      rm -fr dotnet-dev-rhel-x64.latest.tar.gz
      ;;
    freebsd* | darwin* | linux*)
      asdf install dotnet-core 2.1.4
      asdf global dotnet-core 2.1.4
      ;;
  esac
}
get-dotnetcli-by-nix() {
  case "${OSTYPE}" in
    freebsd* | darwin*) ;;
    linux*)
      case $DIST in
        Redhat* | RedHat*)
          # ### mono ###
          nix-install \
            dotnetbuildhelpers \
            mono-${REQUIRED_MONO_VERSION} \
            mono-addins-1.2 \
            mono-dll-fixer \
            mono-zeroconf-0.9.0
          # ### dotnet ###
          wget https://dotnetcli.blob.core.windows.net/dotnet/beta/Binaries/Latest/dotnet-dev-rhel-x64.latest.tar.gz
          sudo yum install -y libicu libuuid libcurl openssl libunwind
          mkdir ~/dotnet
          tar xf dotnet-dev-rhel-x64.latest.tar.gz -C ~/dotnet --verbose
          rm -fr dotnet-dev-rhel-x64.latest.tar.gz
          ;;
        Debian | Ubuntu*)
          case $DIST_VERSION in
            14.04 | 16.04)
              # ### dotnet ###
              sudo sh -c 'echo "deb [arch=amd64] http://apt-mo.trafficmanager.net/repos/dotnet/ trusty main" > /etc/apt/sources.list.d/dotnetdev.list'
              sudo apt-key adv --keyserver apt-mo.trafficmanager.net --recv-keys 417A0893
              sudo apt-get update
              sudo apt-get install dotnet=1.0.0.001598-1
              ;;
          esac
          ;;
      esac
      ;;
  esac
}
set-mono() {
  case "${OSTYPE}" in
    msys | cygwin)
      function csc() {
        cmd /c "csc.exe $*"
      }
      function vbc() {
        cmd /c "vbc.exe $*"
      }
      function jsc() {
        cmd /c "jsc.exe $*"
      }
      function msbuild() {
        cmd /c "MSBuild.exe $*"
      }
      function installutil() {
        cmd /c "InstallUtil.exe $*"
      }
      function ngen() {
        cmd /c "ngen.exe $*"
      }
      ;;
  esac
}

# ----------------------------------------------------------------------
# ### C. Package Manager ###
get-nuget() {
  mkdir -fr ~/.local/NuGet
  wget https://dist.nuget.org/win-x86-commandline/latest/nuget.exe -O ~/.local/NuGet/nuget.exe
  wget http://headsigned.com/download/running-nuget-command-line-on-linux/Microsoft.Build.zip
  unzip Microsoft.Build.zip -d ~/.local/NuGet/
  rm -f Microsoft.Build.zip
}

set-nuget() {
  case $OSTYPE in
    msys | cygwin) function nuget() {
      cmd /c "~/.local/NuGet/nuget.exe $*"
    } ;;
    linux*) alias nuget='mono ~/.local/NuGet/nuget.exe' ;;
  esac
}
if [ ! -f ~/.local/NuGet/nuget.exe ]; then get-nuget; fi
if [ -f ~/.local/NuGet/nuget.exe ]; then set-nuget; fi

# ----------------------------------------------------------------------
# ### D. Scaffolding ###
get-generator-dotnet() {
  npm i -g \
    yo \
    generator-aspnet \
    grunt-init \
    typescript \
    typescript-tools \
    typings
  git clone https://github.com/nosami/grunt-init-csharpsolution.git ~/.grunt-init/csharpsolution
}
case "${OSTYPE}" in
  msys | cygwin) if ! type -p csc >/dev/null; then get-mono; fi ;;
  freebsd* | darwin*) ;;
  linux*)
    case $DIST_VERSION in
      14.04 | 16.04) ;;
      *) if ! type -p mcs >/dev/null; then get-mono; fi ;;
    esac
    ;;
esac
# if ! type -p dotnet > /dev/null ; then get-dotnetcli ; fi

# ----------------------------------------------------------------------
# ### E. Complition ###
get-omnisharp() {
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
  case "${OSTYPE}" in cygwin) cp -f OmniSharp/config-cygwin.json OmniSharp/config.json ;; esac
  case "${OSTYPE}" in
    msys | cygwin) msbuild ;;
    freebsd* | darwin* | linux*) xbuild ;;
  esac
  cd ..
  mv omnisharp-server ~/.local/
}

set-omnisharp() {
  case "${OSTYPE}" in
    msys | cygwin)
      function omnisharp() {
        cmd /c "~/.local/omnisharp-server/OmniSharp/bin/Debug/OmniSharp.exe $*"
      }
      ;;
    freebsd* | darwin* | linux*)
      alias omnisharp='mono ~/.local/omnisharp-server/OmniSharp/bin/Debug/OmniSharp.exe'
      ;;
  esac
}
case "${OSTYPE}" in
  msys | cygwin | freebsd* | darwin*)
    if [ ! -f ~/.local/omnisharp-server/OmniSharp/bin/Debug/OmniSharp.exe ]; then get-omnisharp; fi
    if [ -f ~/.local/omnisharp-server/OmniSharp/bin/Debug/OmniSharp.exe ]; then set-omnisharp; fi
    ;;
  linux*)
    case $DIST_VERSION in
      14.04 | 16.04) ;;
      12.04)
        if [ ! -f ~/.local/omnisharp-server/OmniSharp/bin/Debug/OmniSharp.exe ]; then get-omnisharp; fi
        if [ -f ~/.local/omnisharp-server/OmniSharp/bin/Debug/OmniSharp.exe ]; then set-omnisharp; fi
        ;;
    esac
    ;;
esac

# ----------------------------------------------------------------------
# ### F. Test Runner ###
get-nunit() {
  wget https://github.com/nunit/nunit/releases/download/${REQUIRED_NUNIT_VERSION}/NUnit-${REQUIRED_NUNIT_VERSION}.zip
  unzip NUnit-${REQUIRED_NUNIT_VERSION}.zip -d ~/.local/NUnit
  rm -f NUnit-${REQUIRED_NUNIT_VERSION}.zip
}

set-nunit() {
  case "${OSTYPE}" in
    msys | cygwin)
      function nunit-console() {
        cmd /c "~/.local/NUnit/bin/nunit3-console.exe $*"
      }
      ;;
    freebsd* | darwin* | linux*)
      alias nunit-console='mono ~/.local/NUnit/bin/nunit3-console.exe'
      ;;
  esac
}
case "${OSTYPE}" in
  msys | cygwin | freebsd* | darwin* | linux*)
    if [ ! -f ~/.local/NUnit/bin/nunit3-console.exe ]; then get-nunit; fi
    if [ -f ~/.local/NUnit/bin/nunit3-console.exe ]; then set-nunit; fi
    ;;
esac
