#!/usr/local/bin/fish

# build a static build, then switch back to dynamic
# fix to sort out TH looking for wrong .o files
cabal clean
cabal configure --disable-shared   --disable-executable-dynamic
cabal build --verbose
cabal configure

