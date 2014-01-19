run() {
  cabal clean &&
  cabal configure --enable-tests &&
  cabal build &&
  cabal install 
}

while inotifywait -qq -r -e modify .; do run; echo "Done"; done
