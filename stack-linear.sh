#!/bin/sh
set -uex

. $(pwd)/env.sh
stack \
  --docker \
  --docker-env C_INCLUDE_PATH="$C_INCLUDE_PATH" \
  --docker-env LIBRARY_PATH="$LIBRARY_PATH" \
  --docker-env LD_LIBRARY_PATH="$LD_LIBRARY_PATH" \
  --no-nix \
  $@
