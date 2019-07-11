#!/bin/sh
set -uex

JAVA_PATH="$(dirname "$(java -XshowSettings:properties -version 2>&1 > /dev/null | grep 'java.home' | sed '{s/[[:space:]]*java\.home = //}')")"

export C_INCLUDE_PATH="$JAVA_PATH/include:$JAVA_PATH/include/linux"
export LIBRARY_PATH="$JAVA_PATH/jre/lib/amd64/server"
export LD_LIBRARY_PATH="$JAVA_PATH/jre/lib/amd64/server"
export STACK_YAML=stack-linear.yaml
