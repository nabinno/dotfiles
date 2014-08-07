# BASIC SETTINGS
# ===============
# environment variable
# --------------------
[ -z "$PS1" ] && return
stty -ixon
function parse_git_dirty {
  git diff --no-ext-diff --quiet --exit-code &> /dev/null || echo "*"
}
function parse_git_branch {
  git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e "s/* \(.*\)/(\1$(parse_git_dirty))/"
}
export CLICOLOR=1
export EDITOR='vim -f'
export PS1="\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;36m\]\w\[\033[00m\]\$(parse_git_branch)\$ "
alias ls='ls --color=auto'
alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8
export LANGUAGE=en_US.UTF-8
export LC_CTYPE=UTF-8

export PATH="$HOME/.parts/autoparts/bin:$PATH"
export PATH="$HOME/.cask/bin:$PATH"
export PATH="$HOME/.parts/lib/node_modules/less/bin:$PATH"
eval "$(parts env)"


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
# stty kill undef      # settings for utf8-cygwin
# export LESS=MrXEd    # settings for utf8-cygwin


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



# ALIAS
# =====
zmodload -i zsh/mathfunc
setopt complete_aliases     # aliased ls needs if file/dir completions work
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
alias cvt="/cygdrive/c/progra~2/ImageMagick-6.4.4-Q16/cvt.exe"
alias d='/bin/rm -fr'
alias du="du -h"
alias df="df -h"
# alias e='/cygdrive/c/Progra~2/emacs-23.4/bin/emacsclientw.exe'
alias ee='e d:\.emacs'
alias xyzzy='/cygdrive/d/bin/xyzzy/xyzzycli.exe'
alias E='xyzzy'
alias EE="cd '/cygdrive/c/Progra~2/xyzzy'"
alias e='emacs'
alias grep='egrep -ano'
alias egrep='\egrep -H -n'
function gresreg () {
    for i in $(\grep -lr $1 *) ; do
	cp $i $i.tmp;
	sed -e "s/$1/$2/g" $i.tmp > $i;
	rm $i.tmp;
    done
}
alias ex='/cygdrive/c/Program~2/Microsoft\ Office/OFFICE11/EXCEL.EXE'
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
alias gl='git log --color --graph --pretty=format:"%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset" --abbrev-commit --'
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
alias ij='jobs -l'
alias ic='cat /proc/cpuinfo'
alias in='netstat -a -n | more'
alias im='cat /proc/meminfo'
alias ip="ps -flW"
# alias IP="/cygdrive/c/progra~2/processexplorer/procexp.exe &"
# alias ipp="/cygdrive/c/Progra~2/ProcessExplorer/procexp.exe &"
alias it="date -R"
alias j='cd'
alias k='/bin/mkdir -p'
function kl () { kill -f $1 }
function KL () { cmd /c "taskkill /F /T /IM $1" }

alias l='/cygdrive'
# alias ls='ls'
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

function pmw777 () {
    chown i65c: `find ./$1 -iregex "[^ ]*" -type f -print` ;
    chown i65c: `find ./$1 -iregex "[^ ]*" -type d -print` ;
    chmod 777 `find ./ $1 -iregex "[^ ]*"-type d -print` ;
    chmod 666 `find ./$1 -iregex "[^ ]*" -type f -print` ;
}
function pmw775 () {
    chmod 775 `find ./$1 -iregex "[^ ]*" -type d -print` ;
    chmod 664 `find ./$1 -iregex "[^ ]*" -type f -print` ;
}
function pmw755 () {
    chmod 755 `find ./$1 -iregex "[^ ]*" -type d -print` ;
    chmod 644 `find ./$1 -iregex "[^ ]*" -type f -print` ;
}

alias pp="cd '/cygdrive/c/Progra~2/'"
alias pwd='pwd -P'
function upper () {
    for i in * ; do
        \mv -f $i $(echo $i | tr "[:lower:]" "[:upper:]")
    done
}
# alias putty='/cygdrive/c/progra~2/putty/putty.exe'

# ### put command ###
function put-rsync-normal () { rsync --verbose --exclude-from=/cygdrive/d/.rsync/exclude_000 $4 --delete --checksum --chmod=Fu=rw,g=r,o=r -rltgoD -e "ssh -i ~/.ssh/$1" ~/$1/$3 $2/$3 }
function put-rsync-sakura () { rsync --verbose --exclude-from=~/.rsync/exclude_000 $4 --delete --checksum --chmod=Fu=rw,g=r,o=r -rltgoD -e "ssh -i ~/.ssh/$1" ~/$1/$3 $2@$2.sakura.ne.jp:www/$3 }
function put-lftp-normal () { lftp -c "open -u $2 $3 && cd $4/$5 && lcd ~/$1/www/$5 && mirror -x .svn/.* -x .old/.* -x .*.org.* -e -p -R --use-cache" }
alias prn="put-rsync-normal"
alias prs="put-rsync-sakura"
alias pln="put-lftp-normal"
# alias p101.000="prn 101 edit@ryugaku-mitumori.com:htdocs "
# alias p112.000='pln 112 tanki-ryugaku,7fmffwh328 tanki-ryugaku.sakura.ne.jp www '
# alias p202.800=' \
#      zsh ~/bin/rsync.sh true 202 brand/ $HOME ; \
#      zsh ~/bin/rsync.sh "" 202 tpl/ $HOME ; \
#      '
# alias p901.000="prn 901 i65cbb@i65cbb.sakura.ne.jp:~/www/c4ex.com "
# alias p902.000="prn 902 i65cbb@i65cbb.sakura.ne.jp:~/www/i65c.info "
function put-blahfe () {
    rsync \
	--verbose \
	--exclude-from=/cygdrive/d/.rsync/exclude_blahfe \
	--delete \
	--checksum \
	--chmod=Fu=rw,g=r,o=r \
	-rltgoD \
	~/.blahfe/ ~/google-drive/b_blahfe ;
}
function put-config () {
    rsync \
    	--verbose \
    	--exclude-from=/cygdrive/d/.rsync/exclude_config \
    	--delete \
    	--checksum \
    	--chmod=Fu=rw,g=r,o=r \
    	-rltgoD \
    	~/ ~/google-drive/software_config ;
    # put-rsync-google-drive-blahfe {000..999} ;
}
function put-backup () {
    rsync \
    	--verbose \
    	--delete \
    	--checksum \
    	--chmod=Fu=rw,g=r,o=r \
    	-rltgoD \
    	~/google-drive/ /cygdrive/c/backup/google-drive ;
}
# function put-rsync-to-009-i65c-pc () {
#     rsync \
#     	--verbose \
#     	--exclude-from=/cygdrive/d/.rsync/exclude_config \
#     	--checksum \
#     	-rltD \
#     	~/ //009-i65c-pc/d ;
#     rsync \
#     	--verbose \
#     	--checksum \
#     	--exclude-from=/cygdrive/d/.rsync/exclude_emacs \
#     	--delete \
#     	-rltD \
#     	~/.emacs.d/ //009-i65c-pc/d/.emacs.d ;
#     rsync \
#     	--verbose \
#     	--checksum \
#     	--exclude-from=/cygdrive/d/.rsync/exclude_local \
#     	--delete \
#     	-rltD \
#     	~/local/ //009-i65c-pc/d/local ;
#     # rsync \
#     # 	--verbose \
#     # 	--checksum \
#     # 	--delete \
#     # 	-rltD \
#     # 	"/cygdrive/d/Google Drive/b_blahfe/801/" \
#     # 	"//009-i65c-pc/d/Google Drive/b_blahfe/801" ;
# }
# function put-rsync-blahfe-901-to-901 () {
#     rsync \
#     	--verbose \
#     	--exclude-from=/cygdrive/d/.rsync/exclude_plagger \
#     	--checksum \
#     	-rltD \
# 	-e "ssh -i /cygdrive/d/.ssh/901" \
#     	~/Google\ Drive/b_blahfe/901/plagger i65cbb@i65cbb.sakura.ne.jp:~/ ;
# }
# function put-rsync-blahfe-804-to-804 () {
#     rsync \
#     	--verbose \
#     	--exclude-from=/cygdrive/d/.rsync/exclude_plagger \
#     	--checksum \
#     	-rltD \
# 	-e "ssh -i /cygdrive/d/.ssh/804" \
#     	~/Google\ Drive/b_blahfe/804/plagger sgkk@sgkk.sakura.ne.jp:~/ ;
# }
# function put-blahfe () {
#     if [ ! $2 ] ; then
# 	rsync \
#     	    --verbose \
#     	    --exclude-from=/cygdrive/d/.rsync/exclude_blahfe \
#     	    --checksum \
#     	    -rltD \
# 	    -e "ssh -i /cygdrive/d/.ssh/bg009" \
#     	    ~/.blahfe/$1 blahfe@www4408uj.sakura.ne.jp:~/ ;
#     elif [ $2 ] ; then
# 	rsync \
#     	    --verbose \
#     	    --exclude-from=/cygdrive/d/.rsync/exclude_blahfe \
#     	    --checksum \
#     	    -rltD \
# 	    -e "ssh -i /cygdrive/d/.ssh/bg009" \
#     	    ~/.blahfe/$1/$2 blahfe@www4408uj.sakura.ne.jp:~/$1/ ;
#     fi
# }
function put-802 () {
    /cygdrive/d/bin/s3cmd-1.5.0-beta1/s3cmd \
	sync \
	--delete-removed \
    	--exclude-from /cygdrive/d/.rsync/exclude_802 \
	~/.blahfe/802/www/ s3://www.blahfe.com/ ;
}
# function get-804 () {
#     /usr/bin/rsync \
#     	--verbose \
#     	--exclude-from=/cygdrive/d/.rsync/exclude_804 \
#     	--checksum \
#     	-rltD \
# 	-e "ssh -i /cygdrive/d/.ssh/804" \
#     	sgkk@sgkk.sakura.ne.jp:/home/sgkk ~/.blahfe/804/ ;
# }
function put-805 () {
    /usr/bin/rsync \
    	--verbose \
    	--exclude-from=/cygdrive/d/.rsync/exclude_805 \
    	--checksum \
    	-rltD \
	-e "ssh -i /cygdrive/d/.ssh/805" \
    	~/.blahfe/805 805@www4408uj.sakura.ne.jp:/home/ ;
}
function get-805 () {
    /usr/bin/rsync \
    	--verbose \
    	--exclude-from=/cygdrive/d/.rsync/exclude_805 \
    	--checksum \
    	-rltD \
	-e "ssh -i /cygdrive/d/.ssh/805" \
    	805@www4408uj.sakura.ne.jp:/home/805 ~/.blahfe/ ;
}
# function get-901 () {
#     /usr/bin/rsync \
#     	--verbose \
#     	--exclude-from=/cygdrive/d/.rsync/exclude_901 \
#     	--checksum \
#     	-rltD \
# 	-e "ssh -i /cygdrive/d/.ssh/901" \
#     	i65cbb@i65cbb.sakura.ne.jp:/home/i65cbb ~/.blahfe/901/ ;
# }
# function put-rsync-010-i65c-pc () { prn '' //010-i65c-pc/d$ $1 }
# alias pr010="put-rsync-010-i65c-pc"
# function pr010-all () { for i in {000..999} ; do ; pr010 $i ; done }

alias s3cmd='~/bin/s3cmd-1.5.0-beta1/s3cmd'
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
function scp-wrapper () {
    case $1 in
	get) scp -rq -i ~/.ssh/$2 $3:$4 $5 ;;
	put) scp -rq -i ~/.ssh/$2 $4 $3:$5 ;;
    esac
}
function scp-wrapper2 () {
    case $2 in
	000.064) scp-wrapper $1 $2 inose@192.168.100.64 $3 $4 ;;
	c/401) scp-wrapper $1 $2 sgkk@sgkk.sakura.ne.jp $3 $4 ;;
    esac
}
alias sg="scp-wrapper2 get"
alias sp="scp-wrapper2 put"
alias su="su -l"
alias u='tar zxvf'
alias U='tar zcvf $1.tar.gz $1'
alias uz='unzip'
alias v="cat"
function undonut () {
    /cygdrive/c/progra~1/unDonut/unDonut.exe $1 &
}
alias winrun='exec 'cmd', "/c", ((split '/',$0)[-1], map { s/^(.*)$/(-f $1)?qx{cygpath -w "$1"}:$1/e;chomp;$_; } (@ARGV));'

alias whois="/usr/bin/whois"

function t () { \mv (.*.org*|*.org*|*.tar.gz|*.stackdump|*.tar.gz|*.asx|*.0|*.msi|*.wav|*.doc|*.pdf|$1) .old/ }


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
