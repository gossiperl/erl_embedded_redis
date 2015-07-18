#!/bin/bash
REDIS_VERSION=3.0.3
mkdir -p /tmp/build
cd /tmp/build
wget http://download.redis.io/releases/redis-${REDIS_VERSION}.tar.gz
tar -xvzf redis-${REDIS_VERSION}.tar.gz
cd redis-${REDIS_VERSION}
make
make test || exit

cp /tmp/build/src/redis-server /vagrant/redis-server-${REDIS_VERSION}

echo " \o/ Redis build should now be available in src/"