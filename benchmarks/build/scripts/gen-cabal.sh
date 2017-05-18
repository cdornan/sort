#!/bin/env bash

set -e

usage() {
  echo "usage: $1 (7.8.4|7.10.3|8.0.2)" 1>&2
}

if [ $# -eq 1 ]; then
  vrn=$1
  case ${vrn} in
    7.8.4|7.10.3|8.0.2) ;;
    *) usage $0; exit 1;;
  esac
else
  usage $0; exit 1
fi


mk-freeze() {
  # Generate a cabal.config on stdout from the list of fully-qualified
  # packages on stdin, sorting them in the C locale (so we get ASCII
  # order), reformatting as Cabal version constraints on stdout.
  echo "constraints: base>=0,"
  LC_ALL=C sort | sed s/^/"             "/                \
        | sed 's/-\([^-]*\)$/ ==\1/' | sed '$!s/$/,/'
}

stack --stack-yaml stack-${vrn}.yaml list-dependencies    \
    | egrep -v 'rts|integer-simple'                       \
    | tr ' ' '-'                                          \
    | mk-freeze > build/cabal/cabal-${vrn}.config
