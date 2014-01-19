run() {
  cabal clean &&
  cabal configure --enable-tests &&
  cabal build
}

while inotifywait -qq -r -e modify .; do run; echo "Done"; done
