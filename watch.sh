#!/bin/sh

target=$1
continue=true

packageFiles() {
  find .. -name 'package.yaml' 
}

cabalAndHaskellFiles() {
  find .. -name '*.cabal' -or -name 'cabal.project*' -or -name '*.hs' 
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
    cabalAndHaskellFiles | entr -cd cabal $target -O0
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
