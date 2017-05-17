#!/bin/env bash

set -e

usage() {
  echo "$0 [7.8.4|7.10.3|8.0.2]"
}

if [ "$1" = --help -o $# -ne 0 -a $# -ne 1 ]; then
  usage
  exit 1
fi

if [ $# -eq 0 ]; then
  vrn=7.10.3
else
  vrn=$1
  case ${vrn} in
    7.8.4|7.10.3|8.0.2)
      echo "discarding: cabal.config stack.yaml"
      rm -f cabal.config stack.yaml
      echo "setting up stack.yaml   for ${vrn}"
      cp stack-${vrn}.yaml stack.yaml
      echo "setting up cabal.config for ${vrn}"
      cp build/cabal/cabal-${vrn}.config cabal.config
      echo "You can build with cabal or stack.";;
    *) usage; exit 1;;
  esac
fi
