export REQUIRED_GIT_VERSION=2.11.0

# ----------------------------------------------------------------------
# ### installation ###
get-git() {
  case "${OSTYPE}" in
    freebsd* | darwin*) get-git-flow ;;
    linux*)
      case "${DIST}" in
        Redhat | RedHat)
          sudo yum groupinstall "Development Tools"
          sudo yum install gettext-devel openssl-devel perl-CPAN perl-devel zlib-devel
          wget https://www.kernel.org/pub/software/scm/git/git-${REQUIRED_GIT_VERSION}.tar.gz -O ./git-${REQUIRED_GIT_VERSION}.tar.gz
          wait
          tar zxvf git-${REQUIRED_GIT_VERSION}.tar.gz
          (
            cd git-${REQUIRED_GIT_VERSION}
            ./configure --prefix=/usr/local --with-curl --with-expat
            make prefix=/usr/local all
            sudo make prefix=/usr/local install
          )
          rm -fr git-${REQUIRED_GIT_VERSION} git-${REQUIRED_GIT_VERSION}.tar.gz
          get-git-flow
          ;;
        Debian) ;;
        Ubuntu)
          case "${DIST_VERSION=}" in
            12.04) sudo apt-get install -y python-software-properties ;;
            14.04) sudo apt-get install -y software-properties-common python3-software-properties ;;
          esac
          sudo add-apt-repository ppa:git-core/ppa
          sudo apt-get update -y
          sudo apt-get install -y git
          get-git-flow
          ;;
      esac
      git config --global push.default simple
      ;;
  esac
}

get-git-flow() {
  case "${OSTYPE}" in
    freebsd* | darwin*) port install git-flow ;;
    linux*)
      case "${DIST}" in
        Redhat | RedHat) sudo yum install -y gitflow ;;
        Debian | Ubuntu) sudo apt-get install -y git-flow ;;
      esac
      ;;
  esac
}
if ! type -p git >/dev/null; then
  get-git
else
  _REQUIRED_GIT_VERSION_NUM=$(echo $REQUIRED_GIT_VERSION | sed 's/\(.*\..*\)\..*/\1/')
  _CURRENT_GIT_VERSION=$(git --version 2>&1 | cut -d\  -f 3 | sed 's/\(.*\..*\)\..*/\1/')
  if [[ $_REQUIRED_GIT_VERSION_NUM > $_CURRENT_GIT_VERSION ]]; then get-git; fi
fi

# ----------------------------------------------------------------------
# ### hub ###
export REQUIRED_HUB_VERSION=2.2.8

get-hub() {
  case "${OSTYPE}" in
    freebsd*) ;;
    darwin*) brew install hub ;;
    linux*)
      case "${DIST}" in
        Redhat | RedHat)
          wget https://github.com/github/hub/releases/download/v${REQUIRED_HUB_VERSION}/hub-linux-amd64-${REQUIRED_HUB_VERSION}.tgz
          tar zxvf hub-linux-amd64-${REQUIRED_HUB_VERSION}.tgz
          mv hub-linux-amd64-${REQUIRED_HUB_VERSION}/bin/hub ~/.local/bin/
          rm -fr hub-linux-amd64-${REQUIRED_HUB_VERSION}*
          ;;
      esac
      ;;
  esac
}
if ! type -p hub >/dev/null; then get-hub; fi
if type -p hub >/dev/null; then eval "$(hub alias -s)"; fi

# ----------------------------------------------------------------------
# ### gibo ###
get-gibo() {
  case "${OSTYPE}" in
    freebsd*) ;;
    darwin* | linux*)
      brew install gibo
      ;;
  esac
}
if ! type -p gibo >/dev/null; then get-gibo; fi

# ----------------------------------------------------------------------
# ### bfg ###
get-bfg() {
  REQUIRED_BFG_VERSION=1.12.12
  case "${OSTYPE}" in
    freebsd* | darwin* | linux*)
      wget http://repo1.maven.org/maven2/com/madgag/bfg/${REQUIRED_BFG_VERSION}/bfg-${REQUIRED_BFG_VERSION}.jar -O ~/.local/bin/bfg.jar
      alias bfg='java -jar ~/.local/bin/bfg.jar'
      ;;
  esac
}
if [ ! -f ~/.local/bin/bfg.jar ]; then get-bfg; fi
if [ -f ~/.local/bin/bfg.jar ]; then alias bfg='java -jar ~/.local/bin/bfg.jar'; fi

# ----------------------------------------------------------------------
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

ghclone() {
  git clone git@github.com:${1}.git
}

git-log() {
  \git log ${1} --color --graph --pretty=format:"%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset" --abbrev-commit --
}

git-trend() {
  g trend -n 10
  g trend -l ruby -n 5
  g trend -l elixir -n 5
  g trend -l erlang -n 5
  g trend -l JavaScript -n 5
  g trend -l php -n 3
}

parse-git-dirty() {
  git diff --no-ext-diff --quiet --exit-code &>/dev/null || echo "*"
}

parse-git-branch() {
  git branch --no-color 2>/dev/null | sed -e '/^[^*]/d' -e "s/* \(.*\)/(\1$(parse_git_dirty))/"
}

# ----------------------------------------------------------------------
# github-pull-repositories() {
#   # # Setup Example:
#   # function setup-git-pull-repositories {
#   #     case $1 in
#   #         foo) echo 'npm install';;
#   #         bar) echo 'npm install && bower install';;
#   #     esac
#   # }
#   typeset -A users
#   typeset -A branches
#   typeset -A build_commands
#   while getopts "n:r:" opt; do
#     case $opt in
#       n)
#         dirname=$OPTARG
#         is_dirname='true'
#         ;;
#       r)
#         array=("${(@s:,:)OPTARG}")
#         repository=$array[2]
#         users[$repository]=$array[1]
#         branches[$repository]=$array[3]
#         build_commands[$repository]=$(setup-github-pull-repositories $repository)
#         is_repositoy='true'
#         ;;
#     esac
#   done
#   if [ $# -ge 4 ] && [ $is_dirname ] && [ $is_repositoy ]; then
#     local directory=$(date +%y%m%d)_$dirname
#     local current_pwd=$(pwd)
#     cd ~/
#     mkdir -p ~/${directory}
#     cd ~/${directory}
#     for k in ${(@k)build_commands}; do
#       echo ''
#       echo 'Start git clone:' $k
#       echo '--------------------------------------------------'
#       git clone git@github.com:$users[$k]/$k.git; wait
#       cd ./$k
#       echo ''
#       echo 'Start build:' $build_commands[$k] 'in' $k
#       echo '--------------------------------------------------'
#       git checkout $branches[$k]
#       eval $build_commands[$k]; wait
#       cd ~/${directory}
#     done
#     cd ${current_pwd}
#     echo ''
#     echo ''
#     echo '   _______ __  ____        ____'
#     echo '  / ____(_) /_/ __ \__  __/ / /'
#     echo ' / / __/ / __/ /_/ / / / / / /'
#     echo '/ /_/ / / /_/ ____/ /_/ / / /'
#     echo '\____/_/\__/_/    \__,_/_/_/'
#     echo '                                ....is now installed!'
#   else
#     echo ''
#     echo "Usage: github-pull-repositories -r user1,repository1,branch1 -r user2,repository2,branch2 -n dirname .." 1>&2
#   fi
# }
