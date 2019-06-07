#!/bin/bash
C_INCLUDE_PATH=/usr/lib/jvm/java-8-openjdk-amd64/include:/usr/lib/jvm/java-8-openjdk-amd64/include/linux \
LIBRARY_PATH=/usr/lib/jvm/java-8-openjdk-amd64/jre/lib/amd64/server \
LD_LIBRARY_PATH=/usr/lib/jvm/java-8-openjdk-amd64/jre/lib/amd64/server \
STACK_ROOT=/tmp/inline-java/.stack-root \
STACK_YAML=stack-linear.yaml \
bash
