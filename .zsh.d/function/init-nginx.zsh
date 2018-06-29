get-nginx() {
  case "${OSTYPE}" in
    darwin* | linux*)
      case $DIST in
        Redhat | RedHat)
          sudo yum install epel-release
          sudo yum install nginx
          ;;
        Ubuntu | Debian)
          case $DIST_VERSION in
            14.04 | 16.04) ;;
            *) nix-install nginx ;;
          esac
          ;;
      esac
      ;;
  esac
}

set-nginx() {
  case "${OSTYPE}" in
    darwin* | linux*)
      case $DIST in
        Redhat | RedHat)
          sudo useradd -s /bin/false nginx
          sudo mkdir /etc/nginx/sites-enabled
          sudo mkdir /etc/nginx/sites-available
          sudo mkdir /etc/nginx/conf.d
          ;;
        Ubuntu | Debian)
          case $DIST_VERSION in
            14.04) ;;
            *)
              sudo useradd -s /bin/false nginx
              sudo sed -i "s|^\(#user  nobody;\)|#\1\nuser  nginx;|g" ~/.nix-profile/conf/nginx.conf
              sudo sed -i "s|^\(worker_processes  1;\)|#\1\nworker_processes  auto;|g" ~/.nix-profile/conf/nginx.conf
              sudo sed -i "s|^#\(log_format  main  '$remote_addr - $remote_user [$time_local] \"$request\" '\)|\1|g" ~/.nix-profile/conf/nginx.conf
              sudo sed -i "s|^#\(                  '$status $body_bytes_sent \"$http_referer\" '\)|\1|g" ~/.nix-profile/conf/nginx.conf
              sudo sed -i "s|^#\(                  '\"$http_user_agent\" \"$http_x_forwarded_for\"';\)|\1|g" ~/.nix-profile/conf/nginx.conf
              sudo sed -i "s|^\(#gzip  on;\)|\1\ninclude/etc/nginx/conf.d/\*.conf;\ninclude/etc/nginx/sites-enabled/\*;|g" ~/.nix-profile/conf/nginx.conf
              sudo mkdir /etc/nginx
              sudo mkdir /etc/nginx/sites-enabled
              sudo mkdir /etc/nginx/sites-available
              sudo mkdir /etc/nginx/conf.d
              ;;
          esac
          ;;
      esac
      ;;
  esac
}

# ----------------------------------------------------------------------
nginx-stop() {
  case "${OSTYPE}" in
    darwin* | linux*) sudo nginx -s stop ;;
  esac
}

nginx-restart() {
  case "${OSTYPE}" in
    darwin* | linux*)
      sudo nginx -s stop
      sudo nginx
      nginx-status
      ;;
  esac
}

nginx-status() {
  case "${OSTYPE}" in
    darwin* | linux*)
      sudo nginx -t
      ps aux | grep '[n]ginx'
      ;;
  esac
}
if ! type -p nginx >/dev/null; then get-nginx; fi

alias nk="nginx-stop"
alias nt="nginx-stop"
alias nr="nginx-restart"
alias np="ps aux | grep '[n]ginx'"
alias ns="nginx-status"
