run() {
  cabal clean &&
  cabal configure --enable-tests &&
  cabal build &&
  cabal test
}

while inotifywait -qq -r -e modify .; do run; echo "Done"; done
