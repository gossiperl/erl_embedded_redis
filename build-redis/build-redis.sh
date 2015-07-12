#!/bin/bash
REDIS_VERSION=3.0.2
mkdir -p build
cd build
wget http://download.redis.io/releases/redis-${REDIS_VERSION}.tar.gz
tar -xvzf redis-${REDIS_VERSION}.tar.gz
cd redis-${REDIS_VERSION}
make

echo " \o/ Redis build should now be available in src/"