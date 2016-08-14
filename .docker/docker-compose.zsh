# base
function docker-run-other1 {
    docker run --name other1 -h other1 \
           --network backtier \
           -p 12929:12929 \
           -d kiyoka/sekka
}
function docker-run-cache2 {
    docker run --name cache2 -h cache2 \
           --network backtier \
           -p 11211:11211 \
           -d memcached:1.4
}
function docker-run-cache1 {
    docker run --name cache1 -h cache1 \
           --network backtier \
           -p 6379:6379 \
           -d redis:2.8
}
function docker-run-db3 {
    docker run --name db3 -h db3 \
           --network backtier \
           -p 3307:3306 \
           -e MYSQL_ROOT_PASSWORD=password \
           -d mysql:5.7
    docker cp ~/.docker/etc/mysql/conf.d/my.cnf db3:/etc/mysql/conf.d/my.cnf
    docker exec db3 chown 644 /etc/mysql/conf.d/*
    docker exec db3 /etc/init.d/mysql restart
}
function docker-run-db2 {
    docker run --name db2 -h db2 \
           --network backtier \
           -p 3306:3306 \
           -e MYSQL_ROOT_PASSWORD=password \
           -d mysql:5.6
    docker cp ~/.docker/etc/mysql/conf.d/my.cnf db2:/etc/mysql/conf.d/my.cnf
    docker exec db2 chown 644 /etc/mysql/conf.d/*
    docker exec db2 /etc/init.d/mysql restart
}
function docker-run-db1 {
    docker run --name db1 -h db1 \
           --network backtier \
           -p 5432:5432 \
           -e POSTGRES_PASSWORD=password \
           -d postgres:9.5.3
}
function docker-run-proxy1 {
    docker run --name proxy1 -h proxy1 \
           --network fronttier \
           -p 80:80 \
           -p 443:443 \
           -d nginx:1.10
    docker cp ~/.docker/etc/nginx/nginx.conf proxy1:/etc/nginx/nginx.conf
    docker cp ~/.docker/etc/nginx/conf.d proxy1:/etc/nginx/conf.d
    docker exec proxy1 nginx -s reload
}
function docker-run-app1 {
    docker run --name app1 -h app1 \
           -u action \
           --network fronttier \
           --network backtier \
           -p 2222:22 \
           -p 3000-3009:3000-3009 \
           -p 9000-9009:9000-9009 \
           -d quay.io/nabinno/app \
           sudo /usr/sbin/sshd -D
}

# network
function docker-network-create-backtier {
    docker network create backtier
}
function docker-network-create-fronttier {
    docker network create fronttier
}

# compose
function docker-compose-up {
    for i in $(cat ~/.docker/docker-compose.zsh | grep -e 'function docker-network-create-' | cut -f 2 -d " "); do
        eval "$i"
    done
    for i in $(cat ~/.docker/docker-compose.zsh | grep -e 'function docker-run-' | cut -f 2 -d " "); do
        eval "$i"
    done
}
function docker-compose-restart {
    for i in $(cat ~/.docker/docker-compose.zsh | grep -e 'function docker-run-' | cut -f 2 -d " " | cut -f 3 -d "-"); do
        docker restart $i
    done
}
function docker-compose-stop {
    for i in $(cat ~/.docker/docker-compose.zsh | grep -e 'function docker-run-' | cut -f 2 -d " " | cut -f 3 -d "-"); do
        docker stop $i
    done
}
function docker-compose-rm {
    for i in $(cat ~/.docker/docker-compose.zsh | grep -e 'function docker-run-' | cut -f 2 -d " " | cut -f 3 -d "-"); do
        docker rm -f $i
    done
    for i in $(cat ~/.docker/docker-compose.zsh | grep -e 'function docker-network-create-' | cut -f 2 -d " " | cut -f 4 -d "-"); do
        docker network rm "$i"
    done
}
function dcc {
    eval "docker-compose-$1"
}
