#!/usr/bin/env bash

export GHC_PACKAGE_PATH=$(stack path --ghc-package-path)

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
  yellow 'Usage: ./juvix.sh { repl | build | clean | lint | test | exec | run }'
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
  full)
    if [ -z "$2" ]; then
      usage
      exit 1
    fi
    FILE=$2
    invoke "mkdir -p ./tmp"
    invoke "rm -f ./tmp/*.ibc"
    invoke "stack --docker exec -- idris $FILE.idr -S --ibcsubdir ./tmp -o /dev/null"
    invoke "cp tmp/*.ibc ."
    ./juvix.sh exec transpile $FILE.ibc $FILE.tz
    CODE=$?
    invoke "rm *.ibc"
    exit $CODE
  ;;
  repl)
    invoke "stack --docker ghci"
    exit $?
  ;;
  build)
    invoke "stack --docker build"
    exit $?
  ;;
  clean)
    invoke "stack --docker clean --full"
    exit $?
  ;;
  lint)
    exit 0
    invoke "stack --docker exec -- hlint src app test"
    exit $?
  ;;
  test)
    invoke "stack --docker test"
    exit $?
  ;;
  exec)
    shift
    ARGS=$@
    ./juvix.sh build
    invoke "stack --docker exec -- juvix $ARGS"
    exit $?
  ;;
  run)
    if [ -z "$2" ] || [ -z "$3" ] || [ -z "$4" ]; then
      yellow 'Usage: ./juvix.sh run FILENAME STORAGE PARAMETER [AMOUNT]'
      exit 1
    fi
    TEMPFILE="$(mktemp -p ./tmp)"
    ./juvix.sh exec transpile $2 $TEMPFILE
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
