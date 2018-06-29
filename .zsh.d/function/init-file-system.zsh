# Inotify
REQUIRED_INOTIFY_VERSION=3.14

get-inotify() {
  case "${OSTYPE}" in
    freebsd* | darwin*) ;;
    linux*)
      local current_pwd=$(pwd)
      curl -O http://jensd.be/download/inotify-tools-${REQUIRED_INOTIFY_VERSION}.tar.gz
      tar -xvzf inotify-tools-${REQUIRED_INOTIFY_VERSION}.tar.gz
      cd inotify-tools-${REQUIRED_INOTIFY_VERSION}
      ./configure && make && (yes | sudo make install)
      cd $current_pwd
      rm -fr inotify-tools-${REQUIRED_INOTIFY_VERSION}*
      ;;
  esac
}
if ! type -p inotifywait >/dev/null; then get-inotify; fi

# ### storage minimization ###
delete-log() {
  sudo find /home /var /usr -mtime +1 -a \( -name "*.pag" -o -name "*.dir" -o -name "*.log" \) -exec sudo rm {} \;
}

sync-filesystem() {
  delete-log
  crontab -r
  echo '0 0 * * * sudo find /home /var /usr -mtime +1 -a \( -name "*.pag" -o -name "*.dir" -o -name "*.log" \) -exec sudo rm {} \;' | crontab
}

# ----------------------------------------------------------------------
# Samba
get-samba() {
  case "${OSTYPE}" in
    darwin* | linux*)
      case "${DIST}" in
        Redhat | RedHat | Debian)
          sudo yum remove samba
          sudo yum install samba -y
          set-samba
          ;;
      esac
      ;;
  esac
}

set-samba() {
  case "${OSTYPE}" in
    darwin* | linux*)
      case "${DIST}" in
        Redhat | RedHat | Debian)
          sudo mkdir -p /samba/anonymous_share
          sudo chmod -R 0777 /samba/anonymous_share
          sudo sed -i "s|^\(\[global\]\)|\1\n\tunix charset = UTF-8\n\tdos charset = CP932\n\tmap to guest = Bad User\n|g" /etc/samba/smb.conf
          sudo sed -i "s|workgroup = MYGROUP|workgroup = WORKGROUP|g" /etc/samba/smb.conf
          sudo sed -i "s|^\thosts allow = 127.*/\thosts allow = 127. 192.168.|g" /etc/samba/smb.conf
          sudo sed -i "s|^;\(\tmax protocol = SMB2\)|\1|g" /etc/samba/smb.conf
          sudo sed -i "s|^;\tsecurity = Security|\tsecurity = user|g" /etc/samba/smb.conf
          local smb_conf='
[Anonymous share]
        path = /samba/anonymous_share
        writable = yes
        browsable = yes
        guest ok = yes
        guest only = yes
        create mode = 0777
        directory mode = 0777'
          echo $smb_conf | sudo tee --append /etc/samba/smb.conf
          sudo systemctl start smb
          sudo systemctl start nmb
          sudo systemctl enable smb
          sudo systemctl enable nmb
          ;;
      esac
      ;;
  esac
}
# if ! type -p smbd > /dev/null; then get-samba; fi

samba-restart() {
  case "${OSTYPE}" in
    darwin* | linux*)
      case "${DIST}" in
        Redhat | RedHat | Debian)
          sudo systemctl restart smb
          sudo systemctl restart nmb
          ;;
      esac
      ;;
  esac
}

samba-stop() {
  case "${OSTYPE}" in
    darwin* | linux*)
      case "${DIST}" in
        Redhat | RedHat | Debian)
          sudo systemctl stop smb
          sudo systemctl stop nmb
          ;;
      esac
      ;;
  esac
}

alias sbr=samba-restart
alias sbt=samba-stop
alias sbs=testparm
