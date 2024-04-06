#!/bin/sh

searchDir=$(if  [ -e cabal.project ]; then echo . ; else echo .. ; fi)
command=$1
target=$2
continue=true

packageFiles() {
  find $searchDir -name 'package.yaml' 
}

cabalAndHaskellFiles() {
  find $searchDir -name '*.cabal' -or -name 'cabal.project*' -or -name '*.hs' 
}

hpackWatch() {
  echo hpackWatch
  while [ $continue = true -a -n "$(packageFiles)" ]
  do
    packageFiles | entr -dap hpack /_
  done
}

cabalWatch() {
  echo cabalWatch
  while [ $continue = true ]
  do
    cabalAndHaskellFiles | entr -cd cabal $command -O0 $target
  done
}

shutdown() {
  echo shutdown run
  /bin/kill -s TERM -$$
}
trap shutdown INT

packageFiles | xargs -n 1 hpack
hpackWatch &
bgPID=$!
cabalWatch
