#Thriftier
Wraps a bit of Apache Thrift and makes it a bit easier to use.

[![Build Status](https://travis-ci.org/emchristiansen/thriftier.png)](https://travis-ci.org/emchristiansen/thriftier)

#Getting started
To install, make sure GHC (Haskell) and cabal-install are installed.
On Ubuntu, this looks like:

```
sudo apt-get update
sudo apt-get install ghc cabal-install
```

Then clone this repository and use cabal to install Thriftier.

```
git clone https://github.com/emchristiansen/thriftier
cd thriftier
cabal install --only-dependencies
cabal configure
cabal build
cabal install
```

This will install the executable `thriftier` in `~/.cabal/bin`.
Make sure it's on your PATH.

Now you should be able to run `thriftier --help` for usage instructions.
