# BASIC SETTINGS
# ===============
# environment variable
# --------------------
[ -z "$PS1" ] && return
stty -ixon
stty start undef
stty stop undef
function parse_git_dirty {
  git diff --no-ext-diff --quiet --exit-code &> /dev/null || echo "*"
}
function parse_git_branch {
  git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e "s/* \(.*\)/(\1$(parse_git_dirty))/"
}
# export EDITOR=/usr/local/bin/vi
# export LANG=ja_JP.UTF-8
# export LANG=ja_JP.eucJP
alias egrep='egrep --color=auto'
alias fgrep='fgrep --color=auto'
alias grep='grep --color=auto'
alias ls='ls --color=auto'
export CLICOLOR=1
export EDITOR='vim -f'
export LANG=en_US.UTF-8
export LANGUAGE=en_US.UTF-8
export LC_ALL=en_US.UTF-8
export LC_CTYPE=UTF-8
export LC_MESSAGES=C
export MAILPATH=$HOME/MailBox/postmaster/maildir
export PATH=$HOME/bin:$HOME/local/bin:$PATH
export PATH="$HOME/.parts/autoparts/bin:$PATH"
export PATH="$HOME/.local/bin:$PATH"
export PATH="$HOME/.cask/bin:$PATH"
export PATH="$HOME/.parts/lib/node_modules/less/bin:$PATH"
export PATH="$HOME/.parts/packages/python2/2.7.6/bin:$PATH"
export PATH="$HOME/local/perl-5.18/bin:$PATH"
export PS1="\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;36m\]\w\[\033[00m\]\$(parse_git_branch)\$ "
eval "$(parts env)"

### java ###
export JAVA_HOME=/usr/lib/jvm/java-7-openjdk-amd64
export CLASSPATH=.:$JAVA_HOME/jre/lib:$JAVA_HOME/lib:$JAVA_HOME/lib/tools.jar
export PLAY_HOME=/usr/local/play-2.2.3
export PATH="$JAVA_HOME/bin:$PATH"
export PATH="$PLAY_HOME:$PATH"

# ### perl ###
# eval $(perl -I$HOME/local/lib/perl5 -Mlocal::lib=$HOME/local)
# export PKG_DBDIR=$HOME/local/var/db/pkg
# export PORT_DBDIR=$HOME/local/var/db/pkg
# export INSTALL_AS_USER
# export LD_LIBRARY_PATH=$HOME/local/lib
# export TMPDIR=$HOME/local/tmp
# export MODULEBUILDRC=$HOME/local/.modulebuildrc
# export PERL_MM_OPT="INSTALL_BASE=$HOME/local"
# export PERL5LIB=$HOME/local/lib/perl5:$PERL5LIB
## export PERL_CPANM_OPT="-l ~/local --mirror http://ftp.funet.fi/pub/languages/perl/CPAN/"
# export PERL_CPANM_OPT="-l ~/local --mirror ~/.cpan/minicpan/"

# ### javascript ###
export node='NODE_NO_READLINE=1 node'


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
    xterm)
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
	export LSCOLORS=exfxcxdxbxegedabagacad
	export LS_COLORS='di=34:ln=35:so=32:pi=33:ex=31:bd=46;34:cd=43;34:su=41;30:sg=46;30:tw=42;30:ow=43;30'
	zstyle ':completion:*' list-colors \
	    'di=34' 'ln=35' 'so=32' 'ex=31' 'bd=46;34' 'cd=43;34'
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
if [ ! -f ~/.local/bin/plantuml.jar ]; then
    wget http://jaist.dl.sourceforge.net/project/plantuml/plantuml.8027.jar -O ~/.local/bin/plantuml.jar
fi
alias plantuml='java -jar ~/.local/bin/plantuml.jar -tpng'



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
	;;
    linux*)
	alias ls="ls --color"
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
# ### docker ###
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

# #### git ###
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
alias gl='git log $1 --color --graph --pretty=format:"%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset" --abbrev-commit --'
alias glast='git diff HEAD~1..HEAD'
alias glf='git log --decorate=full --graph --pretty=full'
alias glg='git log --color --graph --pretty=format:"%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset" --abbrev-commit --'
alias gpick='git cherry-pick'
alias gp='git push -v'
alias gpr='git pull --rebase'
alias gs='git status -sb'
alias gsearch=git "!sh -c \'git rev-list --all | grep ^$1 | while read commit; do git --no-pager log -n1 --pretty=format:\"%H %ci %an %s%n\" $commit; done\' -"
alias gslog='git log --graph --pretty=format:"%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset" --abbrev-commit --date=relative'
alias gst='git status -sb'
alias gswitch='git checkout'
alias gvlog='git log --graph --pretty=format:"%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen - %cD (%cr) %C(bold blue)<%an>%Creset%n" --abbrev-commit --date=relative -p ; echo ""'
alias gwho='git shortlog -s --'

# ### dotfiles ###
function get-dotfiles () {
    cd ~/; wait
    if [ ! -d ~/dotfiles ]; then
        git clone https://github.com/nabinno/dotfiles; wait
    fi
    cd ~/dotfiles; wait
    git checkout -- .;  wait
    git pull;  wait
    rm -rf .emacs.d/lisp/*;  wait
    cp -pr ~/.emacs.d/lisp/* .emacs.d/lisp/;  wait
    cp -pr ~/.emacs.d/bin/* .emacs.d/bin/;  wait
    cp -pr ~/.emacs.d/eshell/alias .emacs.d/eshell/;  wait
    cp -pr  ~/.emacs.d/init.el .emacs.d/;  wait
    cp -pr ~/.zshrc .
}
function put-dotfiles () {
    eval current_pwd=`pwd`
    cd ~/
    if [ ! -d ~/dotfiles ]; then
        git clone https://github.com/nabinno/dotfiles; wait
    fi
    cd ~/dotfiles; wait
    git checkout -- .;  wait
    git pull;  wait
    rm -rf ~/.emacs.d/lisp/*;  wait
    cp -pr .emacs.d/lisp/* ~/.emacs.d/lisp/;  wait
    cp -pr .emacs.d/bin/* ~/.emacs.d/bin/;  wait
    cp -pr .emacs.d/eshell/alias ~/.emacs.d/eshell/;  wait
    cp -pr .emacs.d/init.el ~/.emacs.d/;  wait
    cp -pr .zshrc ~/; wait
    cd ${current_pwd}; wait
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
function cpanmodulelist () { perl -e "print \"@INC\"" | find -name "*.pm" -print }
function cpanmoduleversion () { perl -M$1 -le "print \$$1::VERSION" }
# alias cpan-uninstall='perl -MConfig -MExtUtils::Install -e '"'"'($FULLEXT=shift)=~s{-}{/}g;uninstall "$Config{sitearchexp}/auto/$FULLEXT/.packlist",1'"'"
# function cpan-uninstall () {
#     perl -MConfig -MExtUtils::Install -e '($FULLEXT=shift)=~s{-}{/}g;uninstall "$Config{sitearchexp}/auto/$FULLEXT/.packlist",1'
# }
alias cpanmini='cpan --mirror ~/.cpan/minicpan --mirror-only'
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
alias ip="ps -flW"
alias it="date -R"
alias j='cd'
alias k='/bin/mkdir -p'
function kl () { kill -f $1 }
alias la="\ls -p -F -a"
alias ll="\ls -p -F -a"
alias lf="\ls -p -l -F -a"
function lt () {
    \ls -R $1 | \grep ":$" | \sed -e 's/:$//' -e 's/[^-][^\/]*\//--/g' -e 's/^/   /' -e 's/-/|/'
}
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

if [ -f ~/.zshrc.mine ]; then
    source ~/.zshrc.mine
fi
