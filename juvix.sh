#!/usr/bin/env bash

CYAN="\033[0;36m"
RED="\033[0;31m"
YELLOW="\033[0;33m"
GREEN="\033[0;32m"
ENDCOLOR="\033[0m"

cyan() {
  echo -e "$CYAN""$@""$ENDCOLOR"
}

red() {
  echo -e "$RED""$@""$ENDCOLOR"
}

green() {
  echo -e "$GREEN""$@""$ENDCOLOR"
}

yellow() {
  echo -e "$YELLOW""$@""$ENDCOLOR"
}

alphanet() {
  cyan "$PATH_TO_ALPHANET_SH $@"
  $PATH_TO_ALPHANET_SH $@
}

usage() {
  yellow 'Usage: ./juvix.sh { repl | build | clean | lint | test | compile | run }'
}

invoke() {
  cyan $1
  $1
}

ACTION=$1

if [ -z "$ACTION" ]; then
  usage
  exit 1
fi

case $ACTION in
  repl)
    invoke "stack --docker ghci"
    exit $?
  ;;
  build)
    invoke "stack --docker build --install-ghc"
    exit $?
  ;;
  clean)
    invoke "stack --docker clean --full"
    exit $?
  ;;
  compile)
    shift
    ARGS=$@
    ./juvix.sh build
    invoke "stack --docker exec -- juvix compile $ARGS" 
    exit $?
  ;;
  lint)
    exit 0
    invoke "stack --docker exec -- hlint src app test/*.hs"
    exit $?
  ;;
  test)
    invoke "stack --docker test"
    exit $?
  ;;
  run)
    if [ -z "$2" ] || [ -z "$3" ] || [ -z "$4" ]; then
      yellow 'Usage: ./juvix.sh run FILENAME STORAGE PARAMETER [AMOUNT]'
      exit 1
    fi
    TEMPFILE="$(mktemp -p ./tmp)"
    ./juvix.sh compile $2 $TEMPFILE
    EXIT=$?
    if [ $EXIT -ne 0 ]; then
      red 'Compilation failed!'
    else 
      green 'Compilation OK, running program!'
      if [ -z "$5" ]; then
        alphanet client run program container:$TEMPFILE on storage $3 and input $4
      else
        alphanet client run program container:$TEMPFILE on storage $3 and input $4 -amount "$5"
      fi
      EXIT=$?
    fi
    rm $TEMPFILE
    exit $EXIT
  ;;
  *)
    usage
    exit 1
  ;;
esac
