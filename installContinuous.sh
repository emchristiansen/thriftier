while inotifywait -qq -r -e modify .; do cabal install; echo "Done"; done
