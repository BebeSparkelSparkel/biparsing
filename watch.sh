#!/bin/sh

searchDir=$(if  [ -e cabal.project ]; then echo . ; else echo .. ; fi)
command=$1
target=$2
continue=true

packageFiles() {
  find $searchDir -name 'cabal.*' -maxdepth 1
}

cabalFiles() {
  find $searchDir '(' -path '*dist-newstyle*' -prune -or -name '[A-Za-z]*.cabal' ')' -and -type f
}

haskellFiles() {
  find $searchDir '(' -path '*dist-newstyle*' -prune -or -name '[A-Za-z]*.hs' ')' -and -type f
}

haskellDirectories() {
  haskellFiles | xargs -n 1 dirname | sort -u
}

cabalFilesAndHaskellDirectories() {
  cabalFiles
  haskellDirectories
}

cabalGildWatch() {
  echo cabalGildWatch
  while [ $continue = true -a -n "$(cabalFiles)" ]
  do
    cabalFilesAndHaskellDirectories | entr -dap sh -c 'cabal-gild --io=$0 && echo generated $0 && sleep 2 || echo failed generating $0' /_
  done
}

cabalAndHaskellFiles() {
  packageFiles
  cabalFiles
  haskellFiles
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

cabalFiles | xargs -P 20 -n 1 sh -c 'cabal-gild --io=$0 && echo generated $0 || echo failed generating $0'
cabalGildWatch &
bgPID=$!
cabalWatch
