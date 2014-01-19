while inotifywait -qq -r -e modify .; do cabal build; cabal test; echo "Done"; done
