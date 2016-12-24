#
# Base
#
# function docker-run-other1 {
#     docker run --name other1 -h other1 \
#            --network backtier \
#            -p 12929:12929 \
#            -d kiyoka/sekka
# }
# function docker-run-cache2 {
#     docker run --name cache2 -h cache2 \
#            --network backtier \
#            -p 11211:11211 \
#            -d memcached:1.4
# }
# function docker-run-cache1 {
#     docker run --name cache1 -h cache1 \
#            --network backtier \
#            -p 6379:6379 \
#            -d redis:2.8
# }
function docker-run-db3 {
    docker run --name db3 -h db3 \
           --network backtier \
           -p 3307:3306 \
           -e MYSQL_ROOT_PASSWORD=password \
           -d mysql:5.7
    docker cp ~/.docker/etc/mysql/conf.d/my.5.7.cnf db3:/etc/mysql/my.cnf
    docker exec db3 sh -c 'chmod 644 /etc/mysql/my.cnf'
    docker restart db3
}
function docker-run-db2 {
    docker run --name db2 -h db2 \
           --network backtier \
           -p 3306:3306 \
           -e MYSQL_ROOT_PASSWORD=password \
           -d mysql:5.6
    docker cp ~/.docker/etc/mysql/conf.d/my.5.6.cnf db2:/etc/mysql/conf.d/my.cnf
    docker exec db2 sh -c 'chmod 644 /etc/mysql/conf.d/my.cnf'
    docker restart db2
}
function docker-run-db1 {
    docker run --name db1 -h db1 \
           --network backtier \
           -p 5432:5432 \
           -e POSTGRES_PASSWORD=password \
           -d postgres:9.5.3
}
# function docker-run-app1 {
#     docker run --name app1 -h app1 \
#            -u action \
#            --network fronttier \
#            --network backtier \
#            -p 80:80 \
#            -p 2222:22 \
#            -p 3000-3009:3000-3009 \
#            -p 9000-9009:9000-9009 \
#            -d quay.io/nabinno/temp \
#            sudo /usr/sbin/sshd -D
#     docker cp ~/.docker/etc/nginx/nginx.conf app1:/etc/nginx/nginx.conf
#     docker cp ~/.docker/etc/nginx/conf.d app1:/etc/nginx/
#     docker cp ~/.docker/etc/hosts app1:/tmp/hosts
#     docker exec app1 sh -c 'sudo chmod 644 /etc/nginx/nginx.conf'
#     docker exec app1 sh -c 'sudo chmod 644 /etc/nginx/conf.d/*.conf'
#     docker exec app1 sh -c 'sudo rm /etc/nginx/conf.d/default.conf /etc/nginx/sites-available/* /etc/nginx/sites-enabled/*'
#     docker exec app1 sh -c 'cat /tmp/hosts | sudo tee -a /etc/hosts'
#     docker exec app1 sh -c 'sudo nginx'
# }

#
# Network
#
function docker-network-create-backtier  { docker network create backtier }
function docker-network-create-fronttier { docker network create fronttier }

#
# Compose
#
function dcc {
    case $1 in
        up)
            for i in $(cat ~/.docker/docker-compose.zsh | grep -e 'function docker-network-create-' | cut -f 2 -d " "); do
                eval "$i"
            done
            for i in $(cat ~/.docker/docker-compose.zsh | grep -e 'function docker-run-' | cut -f 2 -d " "); do
                eval "$i"
            done
            ;;
        rm)
            for i in $(cat ~/.docker/docker-compose.zsh | grep -e 'function docker-run-' | cut -f 2 -d " " | cut -f 3 -d "-"); do
                docker rm -f $i
            done
            for i in $(cat ~/.docker/docker-compose.zsh | grep -e 'function docker-network-create-' | cut -f 2 -d " " | cut -f 4 -d "-"); do
                docker network rm "$i"
            done
            ;;
        *)
            for i in $(cat ~/.docker/docker-compose.zsh | grep -e 'function docker-run-' | cut -f 2 -d " " | cut -f 3 -d "-"); do
                docker $1 $i
            done
            ;;
    esac
}
