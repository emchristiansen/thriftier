#run() {
  ##set -e
  ##set -o errexit

  #cabal clean || exit 1;
  #cabal configure --enable-tests || exit 1;
  #cabal build || exit 1;
  #cabal test || exit 1;

  ##set +e
#}
run() {
  cabal clean &&
  cabal configure --enable-tests &&
  cabal build &&
  cabal test
}

while inotifywait -qq -r -e modify .; do run; echo "Done"; done
