run() {
  cabal clean &&
  cabal configure \
    --enable-tests \
    --enable-library-profiling \
    --enable-executable-profiling &&
  cabal build &&
  cabal install 
}

while inotifywait -qq -r -e modify .; do run; echo "Done"; done
